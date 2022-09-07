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

#[no_mangle]
unsafe extern "C" fn vlog_startup_routines_bootstrap() {
    let mut cnt = 0;
    while let Some(func) = vlog_startup_routines[cnt] {
        func();
        cnt += 1;
    }
}

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
    encoding: Option<std::collections::HashMap<std::ffi::CString, std::ffi::CString>>,
    // TODO(robin): track change values and only emit on change of time if value changed
    // last_time: Time,
    // last_value: std::ffi::CString,
    // last_tmp: Option<std::ffi::CString>,
}

impl<'a> TraceItem<'a> {
    fn trip(&mut self, time: Time, value: Value<'a>) -> anyhow::Result<PLI_INT32> {
        let sim_time = match time {
            Time::Sim(sim_time) => sim_time,
            _ => unreachable!(),
        };

        self.context.set_time(Some(sim_time));

        let value = match value {
            Value::BinStr(v) => self
                .encoding
                .as_ref()
                .and_then(|e| e.get(v))
                .map(|v| <std::ffi::CString as std::ops::Deref>::deref(v))
                .unwrap_or(v),
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
        //        vpi_control(vpiFinish, 1);
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
                    vpiBinaryConst | vpiDecConst | vpiIntConst => {
                        // rest of args are things to dump
                        for arg in args_iter {
                            #[allow(non_upper_case_globals)]
                            // https://github.com/rust-lang/rust/issues/39371
                            match self.vpi.get(vpiType, Some(arg)) {
                                // TODO(robin): we need more here
                                vpiMemoryWord | vpiModule | vpiGenScope | vpiFunction | vpiTask
                                | vpiNamedBegin | vpiNamedFork | vpiNet | vpiReg
                                | vpiIntegerVar | vpiBitVar | vpiByteVar | vpiLongIntVar
                                | vpiRealVar | vpiNamedEvent | vpiParameter | vpiEnumVar
                                | vpiStructVar | vpiInterface => {}
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
                    "{FST_DUMPFILE} expects a constant argument, got {:?}",
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
            if let Value::String(filename) = self.vpi.value.get(ValueTy::String, filename)? {
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
                match self.vpi.value.get(ValueTy::Int, depth_arg)? {
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
        // the ordering here matters
        static FOR_MODULE: &'static [PLI_INT32] = &[
            vpiInternalScope,
            vpiInterface,
            vpiModule,
            vpiInstance,
            vpiVariables,
            vpiReg,
            vpiNet,
            vpiParameter,
        ];

        static FOR_SCOPE: &'static [PLI_INT32] = &[
            vpiInternalScope,
            vpiInterface,
            vpiModule,
            vpiVariables,
            vpiReg,
            vpiNet,
            vpiParameter,
        ];

        static FOR_BEGIN: &'static [PLI_INT32] =
            &[vpiInternalScope, vpiVariables, vpiReg, vpiParameter];

        #[allow(non_upper_case_globals)]
        match ty {
            vpiModule => FOR_MODULE,
            vpiNamedBegin => FOR_BEGIN,
            vpiGenScope => FOR_SCOPE,
            // TODO(robin): do we want to iterate over modports?
            vpiInterface => {
                static types: &'static [PLI_INT32] = &[vpiNet, vpiVariables, vpiInterface];
                types
            }
            vpiNetArray => {
                static types: &'static [PLI_INT32] = &[vpiNet];
                types
            }
            vpiRegArray => {
                static types: &'static [PLI_INT32] = &[vpiReg];
                types
            }
            vpiPackedArrayVar | vpiPackedArrayNet => {
                static types: &'static [PLI_INT32] = &[vpiElement];
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
        }
        /*
        else if self.vpi.get(511, Some(thing)) == 2 {
            log!("unhandled vhdl thing {:?}", thing);
            Ok(())
        } */
        else if (match self.vpi.get(vpiType, Some(thing)) {
            vpiModule => true,
            _ => false,
        }) && self.vpi.get(vpiProtected, Some(thing)) == 1
        {
            log!("not dumping a protected thing: {:?}", thing);
            Ok(())
        } else {
            // there are two main things we need to differentiate here
            // 1. leaf things for which we actually want to capture the values
            // 2. things that contain sub things which we want to visit recursively until we hit our max_depth
            #[allow(non_upper_case_globals)]
            match self.vpi.get(vpiType, Some(thing)) {
                // a leaf value
                ty @ (vpiNet | vpiLogicNet | vpiParameter | vpiLogicVar | vpiRealVar
                | vpiIntegerVar | vpiTimeVar | vpiEnumVar | vpiEnumNet) => {
                    if let Err(e) = self.add_trace(ty, thing) {
                        log!("error adding var {e:?}");
                    }

                    Ok(())
                }
                // something we can recurse on
                ty @ (vpiModule | vpiStructVar | vpiUnionVar | vpiStructNet | vpiInterface
                | vpiRegArray | vpiNamedBegin | vpiGenScope | vpiPackedArrayVar
                | vpiPackedArrayNet) => {
                    self.with_fst_scope(ty, thing, |this| {
                        let next_depth = if ty == vpiModule {
                            current_depth + 1
                        } else {
                            current_depth
                        };

                        let mut sub_things = Vec::new();
                        for sub_type in Self::subtypes_for(ty) {
                            // log!(
                            //     "iterating type {} of {:?}",
                            //     vpi_const_to_str(*sub_type),
                            //     this.vpi.string.get(vpiFullName, thing)?
                            // );

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
                                    // TODO(robin): cannot print vpiAssignment
                                    // log!(
                                    //     "already iterated over {:?}",
                                    //     this.vpi.string.get(vpiFullName, sub_thing)?
                                    // );
                                }
                            }
                        }

                        Ok(())
                    })
                }
                // TODO(robin): add consts for 3rd party simulator stuff
                vpiPackage | vpiFunction | vpiTask | vpiIntVar | 1024 | vpiTypeParameter => Ok(()),
                ty => Err(anyhow::anyhow!(
                    "don't know how to dump {:?} of type {ty} = {:?}",
                    self.vpi.string.get(vpiName, thing).map(|v| v.to_owned()),
                    self.vpi.string.get(vpiType, thing)
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
            vpiInterface => Ok(fstScopeType::FST_ST_VCD_INTERFACE),
            vpiStructVar | vpiStructNet => Ok(fstScopeType::FST_ST_VCD_STRUCT),
            // TODO(robin): better fit for array scope type?
            vpiRegArray => Ok(fstScopeType::FST_ST_VCD_MODULE),
            vpiPackedArray => Ok(fstScopeType::FST_ST_VCD_MODULE),
            vpiNamedBegin => Ok(fstScopeType::FST_ST_VCD_BEGIN),
            vpiGenScope => Ok(fstScopeType::FST_ST_VCD_GENERATE),
            _ => Err(anyhow::anyhow!(
                "cannot map vpi type {} to fst scope type",
                vpi_const_to_str(ty)
            )),
        }?;
        let name = self.vpi.string.get(vpiName, handle)?.to_owned();

        let unk = std::ffi::CString::new("unk")?;
        let comp_name = match ty {
            vpiModule | vpiInterface => Some(self.vpi.string.get(vpiDefName, handle)?),
            vpiRegArray | vpiNamedBegin | vpiGenScope => None,
            _ => Some(
                self.vpi
                    .string
                    .get(vpiName, self.vpi.handle(vpiTypespec, Some(handle))?)
                    .unwrap_or(&*unk),
            ),
        };

        fst_file.set_scope(scope_type, name, comp_name);
        let ret = func(self);

        let fst_file = self.fst_file.as_mut().unwrap();
        fst_file.set_upscope();

        ret
    }

    fn add_trace(&mut self, ty: PLI_INT32, handle: vpiHandle) -> anyhow::Result<()> {
        // log!("dumping {:?}", self.vpi.string.get(vpiFullName, handle)?);
        let full_name = self.vpi.string.get(vpiFullName, handle)?;
        if self.already_traced_items.contains(full_name) {
            Ok(())
        } else {
            self.already_traced_items.insert(full_name.to_owned());
            // log!(
            //     "WILL TRACE {:?} of type {}",
            //     self.vpi.string.get(vpiFullName, handle),
            //     vpi_const_to_str(ty)
            // );

            #[allow(non_upper_case_globals)]
            // https://github.com/rust-lang/rust/issues/39371
            let var_type = match ty {
                vpiParameter => Ok(fstVarType::FST_VT_VCD_PARAMETER),
                vpiLogicVar => Ok(fstVarType::FST_VT_SV_LOGIC),
                vpiNet => match self.vpi.get(vpiNetType, Some(handle)) {
                    vpiWire => Ok(fstVarType::FST_VT_VCD_WIRE),
                    vpiUwire => Ok(fstVarType::FST_VT_VCD_WIRE),
                    vpiTri0 => Ok(fstVarType::FST_VT_VCD_TRI0),
                    vpiTri1 => Ok(fstVarType::FST_VT_VCD_TRI1),
                    vpiTri => Ok(fstVarType::FST_VT_VCD_TRI),
                    unk => Err(anyhow::anyhow!("unhandled net type {unk}")),
                },
                vpiRealVar => Ok(fstVarType::FST_VT_VCD_REAL),
                vpiIntegerVar => Ok(fstVarType::FST_VT_VCD_INTEGER),
                vpiTimeVar => Ok(fstVarType::FST_VT_VCD_TIME),
                vpiEnumVar | vpiEnumNet => Ok(fstVarType::FST_VT_SV_ENUM),
                unk => Err(anyhow::anyhow!(
                    "unhandled variable type {}",
                    vpi_const_to_str(unk)
                )),
            }?;

            let fst_file = self.fst_file.as_mut().unwrap();

            // TODO(robin): store these somewhere and then reuse?
            let (encoding, bits): (Option<std::collections::HashMap<_, _>>, _) =
                if ty == vpiEnumVar || ty == vpiEnumNet {
                    let typespec = self.vpi.handle(vpiTypespec, Some(handle))?;
                    let mut names = Vec::new();
                    let mut values = Vec::new();

                    let anon = std::ffi::CString::new("anonymous")?;
                    let name = self
                        .vpi
                        .string
                        .get(vpiName, typespec)
                        .unwrap_or(&*anon)
                        .to_owned();
                    for enum_const in self.vpi.iter(vpiEnumConst, Some(typespec))? {
                        names.push(self.vpi.string.get(vpiName, enum_const)?.to_owned());
                        values.push(match self.vpi.value.get(ValueTy::BinStr, enum_const) {
                            Ok(Value::BinStr(s)) => s.to_owned(),
                            _ => unreachable!(),
                        });
                    }

                    let mut one_hot_values = Vec::new();
                    let bits = values.len();
                    for i in 0..bits {
                        one_hot_values.push(std::ffi::CString::new(format!(
                            "{:0width$b}",
                            1 << i,
                            width = bits
                        ))?);
                    }
                    let enum_handle = fst_file.create_enum_table(
                        name,
                        names.into_iter().zip(one_hot_values.clone().into_iter()),
                    );
                    fst_file.emit_enum_table_ref(enum_handle);

                    // TODO(robin): add appropriate sized x and z entries in the encoding

                    (
                        Some(values.into_iter().zip(one_hot_values.into_iter()).collect()),
                        bits as _,
                    )
                } else {
                    (None, self.vpi.get(vpiSize, Some(handle)).try_into()?)
                };

            let fst_handle = fst_file.create_var(
                var_type,
                fstVarDir::FST_VD_IMPLICIT,
                bits,
                self.vpi.string.get(vpiName, handle)?,
                None,
            );

            let value = match self.vpi.value.get(ValueTy::BinStr, handle) {
                Ok(Value::BinStr(v)) => encoding
                    .as_ref()
                    .and_then(|e| e.get(v))
                    .map(|v| <std::ffi::CString as std::ops::Deref>::deref(v))
                    .unwrap_or(v),
                Err(_) => {
                    return Err(anyhow::anyhow!(
                        "got no value for {:?}",
                        self.vpi.string.get(vpiFullName, handle)?
                    ))
                }
                _ => unreachable!(),
            };
            self.fst_file
                .as_mut()
                .unwrap()
                .emit_value_change(fst_handle, value);

            // no need / illegal to register callbacks on constant objects
            if ty != vpiParameter {
                let cheated_context_ref = unsafe { &mut *(self as *mut Context) };
                let trace_item = self.trace_items.alloc(TraceItem {
                    context: cheated_context_ref,
                    handle,
                    fst_handle,
                    ty,
                    encoding,
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

            Ok(())
        }
    }
}

unsafe extern "C" fn value_changed_handler(data: *mut t_cb_data) -> PLI_INT32 {
    let trace_item = &mut *((*data).user_data as *mut TraceItem);
    catch_error(|| {
        trace_item.trip(
            Time::from(*(*data).time),
            Value::try_from(*(*data).value).unwrap(),
        )
    })
}

unsafe fn alloc_inited<T>(init_value: T) -> *mut T {
    let ptr = std::alloc::alloc(std::alloc::Layout::new::<T>());
    // use ptr::write to avoid drop being called on the uninitialized T being overwritten
    std::ptr::write(ptr as *mut T, init_value);

    ptr as *mut T
}

// TODO(robin):
// enums
// vhdl interop
// time vars
// real vars
