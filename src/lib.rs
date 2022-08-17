#[allow(non_upper_case_globals)]
use std::ffi::CStr;

use fst_sys::{fstHandle, fstScopeType, fstVarDir, fstVarType};
use vpi_sys::*;

mod fst;
mod vpi;

use fst::*;
use vpi::{Time, VPIContext, Value, ValueTy};

const FST_DUMPFILE: &str = "$fstDumpfile";
const FST_DUMPVARS: &str = "$fstDumpvars";

// for valgrind
use std::alloc::System;

use crate::vpi::{vpi_const_to_str, TimeTy};

#[global_allocator]
static A: System = System;

#[allow(non_upper_case_globals)]
#[no_mangle]
pub static vlog_startup_routines: [Option<extern "C" fn()>; 2] = [Some(fst_register), None];

macro_rules! log {
    ($($args:expr),*) => {
        let message = format!($($args),*);
        let format_part = std::ffi::CString::new("FST: %s\n").unwrap();
        let formatted_part = std::ffi::CString::new(message).unwrap();
        #[allow(unused_unsafe)]
        unsafe {
            vpi_printf(format_part.as_ptr() as _, formatted_part.as_ptr());
        }
    }
}

type SimTime = u64;

#[derive(Debug)]
struct TraceItem<'a> {
    context: &'a mut Context,
    handle: vpiHandle,
    fst_handle: fstHandle,
    ty: PLI_INT32,
}

impl<'a> TraceItem<'a> {
    fn trip(&mut self, time: Time, value: Value<'a>) -> anyhow::Result<PLI_INT32> {
        let sim_time = match time {
            Time::Sim(sim_time) => sim_time,
            _ => unreachable!(),
        };
        self.context.set_time(Some(sim_time));

        let value = match value {
            Value::BinStr(v) => v,
            _ => unreachable!(),
        };
        self.context
            .fst_file
            .as_mut()
            .unwrap()
            .emit_value_change(self.fst_handle, value);

        Ok(0)
    }
}

#[derive(Debug)]
struct Context {
    fst_file: Option<FstFile>,
    vpi: VPIContext,
    current_time: u64,
    trace_items: bumpalo::Bump,
    already_traced_items: std::collections::HashSet<std::ffi::CString>,
}

impl Context {
    // Safety: only allowed to be created once
    unsafe fn new() -> Self {
        Self {
            fst_file: None,
            vpi: VPIContext::new(),
            current_time: 0,
            trace_items: bumpalo::Bump::new(),
            already_traced_items: std::collections::HashSet::new(),
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        if self.fst_file.is_some() {
            self.set_time(None);
        }
    }
}

// unsafe bootstrapping
pub extern "C" fn fst_register() {
    let mut error_handler_data = t_cb_data {
        reason: cbError,
        cb_rtn: Some(error_handler),
        ..Default::default()
    };
    unsafe {
        vpi_register_cb(&mut error_handler_data as _);
        error_handler_data.reason = cbPLIError;
        vpi_register_cb(&mut error_handler_data as _);

        let user_data = alloc_inited(Context::new());
        let name = std::ffi::CString::new(FST_DUMPFILE).unwrap();
        let mut dumpfile_config = t_vpi_systf_data {
            type_: vpiSysTask,
            tfname: name.as_ptr() as _,
            calltf: Some(fst_dumpfile_calltf),
            compiletf: Some(fst_dumpfile_compiletf),
            sizetf: None,
            user_data: user_data as _,
            ..Default::default()
        };
        vpi_register_systf(&mut dumpfile_config as _);

        let name = std::ffi::CString::new(FST_DUMPVARS).unwrap();
        let mut dumpvars_config = t_vpi_systf_data {
            type_: vpiSysTask,
            tfname: name.as_ptr() as _,
            calltf: Some(fst_dumpvars_calltf),
            compiletf: Some(fst_dumpvars_compiletf),
            sizetf: None,
            user_data: user_data as _,
            ..Default::default()
        };
        vpi_register_systf(&mut dumpvars_config as _);

        let mut cleanup_handler_data = t_cb_data {
            reason: cbEndOfSimulation,
            cb_rtn: Some(cleanup),
            user_data: user_data as _,
            ..Default::default()
        };
        vpi_register_cb(&mut cleanup_handler_data as _);
    }
}

unsafe extern "C" fn error_handler(_: *mut t_cb_data) -> PLI_INT32 {
    let mut error_info: t_vpi_error_info = std::mem::zeroed();
    let level = vpi_chk_error(&mut error_info as *mut _);

    if level != 0 {
        log!(
            "ERROR: {:?} at {:?}:{}\n",
            CStr::from_ptr(error_info.message),
            CStr::from_ptr(error_info.file),
            error_info.line
        );
        vpi_control(vpiFinish, 1);
    }

    0
}

unsafe extern "C" fn cleanup(data: *mut t_cb_data) -> PLI_INT32 {
    let context = (*data).user_data as *mut Context;
    std::ptr::drop_in_place(context);
    std::alloc::dealloc((*data).user_data as _, std::alloc::Layout::new::<Context>());

    0
}

unsafe extern "C" fn fst_dumpfile_calltf(user_data: *mut PLI_BYTE8) -> PLI_INT32 {
    let context = &mut *(user_data as *mut Context);
    catch_error(|| context.dumpfile_calltf())
}

unsafe extern "C" fn fst_dumpfile_compiletf(user_data: *mut PLI_BYTE8) -> PLI_INT32 {
    let context = &mut *(user_data as *mut Context);
    catch_error(|| context.dumpfile_compiletf())
}

unsafe extern "C" fn fst_dumpvars_calltf(user_data: *mut PLI_BYTE8) -> PLI_INT32 {
    let context = &mut *(user_data as *mut Context);
    catch_error(|| context.dumpvars_calltf())
}

unsafe extern "C" fn fst_dumpvars_compiletf(user_data: *mut PLI_BYTE8) -> PLI_INT32 {
    let context = &mut *(user_data as *mut Context);
    catch_error(|| context.dumpvars_compiletf())
}

fn catch_error<T: Default>(func: impl FnOnce() -> anyhow::Result<T>) -> T {
    match func() {
        Ok(v) => v,
        Err(e) => {
            log!("Error: {:?}", e);
            unsafe { vpi_control(vpiFinish, 1) };
            Default::default()
            // TODO(robin): implement cleanup here?
        }
    }
}

impl Context {
    fn dumpfile_compiletf(&mut self) -> anyhow::Result<PLI_INT32> {
        let call_handle = self.vpi.handle(vpiSysTfCall, None)?;
        let mut args_iter = self.vpi.iter(vpiArgument, Some(call_handle))?;
        let potential_filename = args_iter.next().ok_or(anyhow::anyhow!(
            "{FST_DUMPFILE} expects exactly one argument"
        ))?;
        if args_iter.next() == None {
            #[allow(non_upper_case_globals)] // https://github.com/rust-lang/rust/issues/39371
            match self.vpi.get(vpiType, Some(potential_filename)) {
                vpiConstant | vpiParameter => {
                    match self.vpi.get(vpiConstType, Some(potential_filename)) {
                        vpiStringConst => Ok(0),
                        _ => Err(anyhow::anyhow!(
                            "{FST_DUMPFILE} expects a constant string argument, got {:?}",
                            self.vpi.string.get(vpiConstType, potential_filename),
                        )),
                    }
                }
                _ => Err(anyhow::anyhow!(
                    "{FST_DUMPFILE} expects a constant string argument, got {:?}",
                    self.vpi.string.get(vpiType, potential_filename),
                )),
            }
        } else {
            Err(anyhow::anyhow!(
                "{FST_DUMPFILE} expects exactly one argument"
            ))
        }
    }

    fn dumpvars_compiletf(&mut self) -> anyhow::Result<PLI_INT32> {
        let call_handle = self.vpi.handle(vpiSysTfCall, None)?;
        let mut args_iter = self.vpi.iter(vpiArgument, Some(call_handle))?;
        // [depth]
        if let Some(depth_arg) = args_iter.next() {
            #[allow(non_upper_case_globals)] // https://github.com/rust-lang/rust/issues/39371
            match self.vpi.get(vpiType, Some(depth_arg)) {
                vpiConstant | vpiParameter => match self.vpi.get(vpiConstType, Some(depth_arg)) {
                    vpiBinaryConst | vpiIntConst => {
                        // rest of args are things to dump
                        for arg in args_iter {
                            #[allow(non_upper_case_globals)]
                            // https://github.com/rust-lang/rust/issues/39371
                            match self.vpi.get(vpiType, Some(arg)) {
                                // TODO(robin): we need more here
                                vpiMemoryWord | vpiModule | vpiGenScope | vpiFunction | vpiTask
                                | vpiNamedBegin | vpiNamedFork | vpiNet | vpiReg
                                | vpiIntegerVar | vpiBitVar | vpiByteVar | vpiLongIntVar
                                | vpiRealVar | vpiNamedEvent | vpiParameter => {}
                                _ => {
                                    return Err(anyhow::anyhow!(
                                        "{FST_DUMPVARS} cannot dump {:?}",
                                        self.vpi.string.get(vpiType, arg)?
                                    ))
                                }
                            }
                        }
                        Ok(0)
                    }
                    _ => Err(anyhow::anyhow!(
                        "{FST_DUMPVARS} expects a constant int argument, got {:?}",
                        self.vpi.get(vpiConstType, Some(depth_arg)),
                    )),
                },
                _ => Err(anyhow::anyhow!(
                    "{FST_DUMPFILE} expects a constant int argument, got {:?}",
                    self.vpi.string.get(vpiType, depth_arg),
                )),
            }
        } else {
            Ok(0)
        }
    }

    fn dumpfile_calltf(&mut self) -> anyhow::Result<PLI_INT32> {
        if self.fst_file.is_none() {
            let filename = self
                .vpi
                .iter(vpiArgument, Some(self.vpi.handle(vpiSysTfCall, None)?))?
                .next()
                .unwrap();
            if let Value::String(filename) = self.vpi.value.get(ValueTy::String, filename) {
                let filename = filename.to_str()?.to_owned();
                let filename = if filename.ends_with(".fst") {
                    filename
                } else {
                    filename + ".fst"
                };

                let mut fst_file = FstFile::new(&filename, true);
                fst_file.set_timescale(self.vpi.get(vpiTimeUnit, None));
                self.fst_file = Some(fst_file);
                self.set_time_forced(None);

                Ok(0)
            } else {
                Err(anyhow::anyhow!(
                    "{FST_DUMPFILE}: did not get a string argument"
                ))
            }
        } else {
            Err(anyhow::anyhow!(
                "{FST_DUMPFILE}: repeated call to {FST_DUMPFILE}"
            ))
        }
    }

    fn dumpvars_calltf(&mut self) -> anyhow::Result<PLI_INT32> {
        if self.fst_file.is_some() {
            let call_handle = self.vpi.handle(vpiSysTfCall, None)?;
            let mut args_iter = self.vpi.iter(vpiArgument, Some(call_handle))?;

            // parse (optional) depth
            let depth = if let Some(depth_arg) = args_iter.next() {
                match self.vpi.value.get(ValueTy::Int, depth_arg) {
                    Value::Int(depth) => {
                        if depth == 0 {
                            i32::MAX
                        } else {
                            depth
                        }
                    }
                    v => {
                        return Err(anyhow::anyhow!(
                            "{FST_DUMPVARS}: expected integer depth argument, got {:?}",
                            v
                        ))
                    }
                }
            } else {
                i32::MAX
            };

            // if we now have further arguments these are the paths we need to dump
            let mut specific_paths = false;
            for arg in args_iter {
                specific_paths = true;

                self.move_to_scope(arg, |this| this.add_dumpvar(arg, depth, 0))?;
            }

            // if we did not have any further arguments, dump everything
            if !specific_paths {
                for top_module in self.vpi.iter(vpiInstance, None)? {
                    self.add_dumpvar(top_module, depth, 0)?;
                }
            }

            log!("dumping with max depth {depth}");

            Ok(0)
        } else {
            Err(anyhow::anyhow!(
                "{FST_DUMPVARS} called before {FST_DUMPFILE}"
            ))
        }
    }

    fn move_to_scope<T>(
        &mut self,
        thing: vpiHandle,
        func: impl FnOnce(&mut Self) -> anyhow::Result<T>,
    ) -> anyhow::Result<T> {
        let mut scope_stack = Vec::new();
        let mut up = thing;
        while let Ok(upper) = self.vpi.handle(vpiScope, Some(up)) {
            up = upper;
            scope_stack.push((self.vpi.get(vpiType, Some(upper)), upper));
        }

        self.move_to_scope_inner(scope_stack, func)
    }

    fn move_to_scope_inner<T>(
        &mut self,
        mut scope_stack: Vec<(PLI_INT32, vpiHandle)>,
        func: impl FnOnce(&mut Self) -> anyhow::Result<T>,
    ) -> anyhow::Result<T> {
        match scope_stack.pop() {
            Some((ty, handle)) => self.with_fst_scope(ty, handle, |this| {
                this.move_to_scope_inner(scope_stack, func)
            }),
            None => func(self),
        }
    }

    fn set_time(&mut self, time: Option<SimTime>) {
        self.set_time_inner(time, false)
    }

    fn set_time_forced(&mut self, time: Option<SimTime>) {
        self.set_time_inner(time, true)
    }

    fn set_time_inner(&mut self, time: Option<SimTime>, force: bool) {
        let time = time.unwrap_or_else(|| match self.vpi.get_time(TimeTy::Sim, None) {
            Time::Sim(time) => time,
            _ => unreachable!(),
        });
        if (self.current_time != time) || force {
            self.fst_file.as_mut().unwrap().emit_time_change(time);
            self.current_time = time;
        }
    }

    fn subtypes_for(ty: PLI_INT32) -> &'static [PLI_INT32] {
        static FOR_MODULE: &'static [PLI_INT32] = &[
            vpiInterface,
            vpiModule,
            vpiInstance,
            vpiVariables,
            vpiReg,
            vpiNet,
            vpiParameter,
            vpiMember,
        ];

        #[allow(non_upper_case_globals)]
        match ty {
            vpiModule => FOR_MODULE,
            vpiNetArray => {
                static types: &'static [PLI_INT32] = &[vpiNet];
                types
            }
            vpiRegArray => {
                static types: &'static [PLI_INT32] = &[vpiReg];
                types
            }
            vpiStructVar | vpiUnionVar | vpiStructNet => {
                static types: &'static [PLI_INT32] = &[vpiMember];
                types
            }
            _ => panic!("dont know how to iterate {}", vpi_const_to_str(ty)),
        }
    }

    // TODO(robin): can we somehow determine the direction for nets?
    // we can only get direction for ports and there seems to be no clear way from ports to nets?
    // we can use vpiLowConn to get a port from a net (upwards) -> get the direction :)
    fn add_dumpvar(
        &mut self,
        thing: vpiHandle,
        max_depth: i32,
        current_depth: i32,
    ) -> anyhow::Result<()> {
        if max_depth == current_depth {
            Ok(())
        } else {
            // there are two main things we need to differentiate here
            // 1. leaf things for which we actually want to capture the values
            // 2. things that contain sub things which we want to visit recursively until we hit our max_depth
            #[allow(non_upper_case_globals)]
            match self.vpi.get(vpiType, Some(thing)) {
                // a leaf value
                ty @ (vpiNet | vpiLogicNet | vpiParameter | vpiLogicVar) => {
                    self.add_trace(ty, thing)?;

                    Ok(())
                }
                // something we can recurse on
                ty @ (vpiModule | vpiStructVar | vpiUnionVar | vpiStructNet) => self
                    .with_fst_scope(ty, thing, |this| {
                        let next_depth = if ty == vpiModule {
                            current_depth + 1
                        } else {
                            current_depth
                        };

                        let mut sub_things = Vec::new();
                        for sub_type in Self::subtypes_for(ty) {
                            log!(
                                "iterating type {} of {:?}",
                                vpi_const_to_str(*sub_type),
                                this.vpi.string.get(vpiFullName, thing)?
                            );

                            for sub_thing in this.vpi.maybe_empty_iter(*sub_type, Some(thing)) {
                                if sub_things
                                    .iter()
                                    .position(|other_thing| {
                                        this.vpi.compare_objects(sub_thing, *other_thing)
                                    })
                                    .is_none()
                                {
                                    sub_things.push(sub_thing);
                                    this.add_dumpvar(sub_thing, max_depth, next_depth)?;
                                } else {
                                    log!(
                                        "already iterated over {:?}",
                                        this.vpi.string.get(vpiFullName, thing)?
                                    );
                                }
                            }
                        }

                        Ok(())
                    }),
                _ => Err(anyhow::anyhow!(
                    "don't know how to dump {:?} of type {:?}",
                    self.vpi.string.get(vpiName, thing)?.to_owned(),
                    self.vpi.string.get(vpiType, thing)?
                )),
            }
        }
    }

    fn with_fst_scope<T>(
        &mut self,
        ty: PLI_INT32,
        handle: vpiHandle,
        func: impl FnOnce(&mut Self) -> anyhow::Result<T>,
    ) -> anyhow::Result<T> {
        let fst_file = self.fst_file.as_mut().unwrap();
        #[allow(non_upper_case_globals)]
        // https://github.com/rust-lang/rust/issues/39371
        let scope_type = match ty {
            vpiModule => Ok(fstScopeType::FST_ST_VCD_MODULE),
            _ => Err(anyhow::anyhow!(
                "cannot map vpi type {} to fst scope type",
                vpi_const_to_str(ty)
            )),
        }?;
        let name = self.vpi.string.get(vpiName, handle)?.to_owned();
        let comp_name = self.vpi.string.get(vpiDefName, handle)?;
        fst_file.set_scope(scope_type, name, Some(comp_name));
        let ret = func(self);

        let fst_file = self.fst_file.as_mut().unwrap();
        fst_file.set_upscope();

        ret
    }

    fn add_trace(&mut self, ty: PLI_INT32, handle: vpiHandle) -> anyhow::Result<()> {
        let full_name = self.vpi.string.get(vpiFullName, handle)?;
        if self.already_traced_items.contains(full_name) {
            Ok(())
        } else {
            self.already_traced_items.insert(full_name.to_owned());
            log!(
                "WILL TRACE {:?} of type {}",
                self.vpi.string.get(vpiFullName, handle),
                vpi_const_to_str(ty)
            );

            #[allow(non_upper_case_globals)]
            // https://github.com/rust-lang/rust/issues/39371
            let var_type = match ty {
                vpiParameter => Ok(fstVarType::FST_VT_VCD_PARAMETER),
                vpiLogicVar => Ok(fstVarType::FST_VT_SV_LOGIC),
                vpiNet => match self.vpi.get(vpiNetType, Some(handle)) {
                    vpiWire => Ok(fstVarType::FST_VT_VCD_WIRE),
                    unk => Err(anyhow::anyhow!("unhandled net type {unk}")),
                },
                unk => Err(anyhow::anyhow!(
                    "unhandled variable type {}",
                    vpi_const_to_str(unk)
                )),
            }?;

            let fst_file = self.fst_file.as_mut().unwrap();
            let fst_handle = fst_file.create_var(
                var_type,
                fstVarDir::FST_VD_IMPLICIT,
                self.vpi.get(vpiSize, Some(handle)).try_into()?,
                self.vpi.string.get(vpiName, handle)?,
                None,
            );

            {
                let cheated_context_ref = unsafe { &mut *(self as *mut Context) };
                let trace_item = self.trace_items.alloc(TraceItem {
                    context: cheated_context_ref,
                    handle,
                    fst_handle,
                    ty,
                });

                let mut time_config = t_vpi_time {
                    type_: TimeTy::Sim as _,
                    ..Default::default()
                };

                // TODO(robin): this can probably be optimized by not always using BinStr, but using vector / scalar depending on size and 4 value logic
                let mut value_config = t_vpi_value {
                    format: ValueTy::BinStr as _,
                    ..Default::default()
                };

                let mut value_change_cb = t_cb_data {
                    reason: cbValueChange,
                    obj: handle,
                    cb_rtn: Some(value_changed_handler),
                    user_data: trace_item as *mut _ as *mut i8,
                    time: &mut time_config as *mut _,
                    value: &mut value_config as *mut _,
                    ..Default::default()
                };
                unsafe { vpi_register_cb(&mut value_change_cb as _) };
            }

            let value = match self.vpi.value.get(ValueTy::BinStr, handle) {
                Value::BinStr(v) => v,
                _ => unreachable!(),
            };
            self.fst_file
                .as_mut()
                .unwrap()
                .emit_value_change(fst_handle, value);

            Ok(())
        }
    }
}

unsafe extern "C" fn value_changed_handler(data: *mut t_cb_data) -> PLI_INT32 {
    let trace_item = &mut *((*data).user_data as *mut TraceItem);
    catch_error(|| trace_item.trip(Time::from(*(*data).time), Value::from(*(*data).value)))
}

unsafe fn alloc_inited<T>(init_value: T) -> *mut T {
    let ptr = std::alloc::alloc(std::alloc::Layout::new::<T>());
    // use ptr::write to avoid drop being called on the uninitialized T being overwritten
    std::ptr::write(ptr as *mut T, init_value);

    ptr as *mut T
}

// TODO(robin):
// enums
