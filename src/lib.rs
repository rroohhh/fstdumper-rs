#![allow(non_upper_case_globals)]
use std::ffi::{CStr, CString};
use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use fst_sys::{fstHandle, fstScopeType, fstVarDir, fstVarType};
use vhpi_sys::{
    vhpi0, vhpi1, vhpiCbDataS, vhpiCbDataT, vhpiCbValueChange, vhpiClassKindT, vhpiDontCare,
    vhpiEnumT, vhpiFormatT, vhpiH, vhpiHandleT, vhpiIntPropertyT, vhpiIntT, vhpiL, vhpiOneToManyT,
    vhpiOneToOneT, vhpiStrPropertyT, vhpiTimeT, vhpiTrue, vhpiU, vhpiValueS,
    vhpiValueS__bindgen_ty_1, vhpiW, vhpiX, vhpiZ, vhpi_register_cb, PLI_VOID,
};
use vpi_sys::*;

mod fst;
mod vhpi;
mod vpi;

use fst::*;
use vhpi::VHPIContext;
use vpi::{vpiLanguage, vpiVHDL, vpiVerilog, Time, VPIContext, Value, ValueTy};

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
        let format_part = CString::new("FST: %s\n").unwrap();
        let formatted_part = CString::new(message).unwrap();
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
    fst_handle: fstHandle,
    encoding: Option<HashMap<CString, CString>>,
    is_string: bool,
    // TODO(robin): track change values and only emit on change of time if value changed
    // last_time: Time,
    // last_value: CString,
    // last_tmp: Option<CString>,
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
                .map(<CString as Deref>::deref)
                .unwrap_or(v),
            _ => unreachable!(),
        };
        if self.is_string {
            self.context
                .fst_file
                .as_mut()
                .unwrap()
                .emit_var_length_value_change(self.fst_handle, value);
        } else {
            self.context
                .fst_file
                .as_mut()
                .unwrap()
                .emit_value_change(self.fst_handle, value);
        }

        Ok(0)
    }
}

#[derive(Debug)]
struct Context {
    fst_file: Option<FstFile>,
    vpi: VPIContext,
    vhpi: VHPIContext,
    current_time: u64,
    trace_items: bumpalo::Bump,
    already_traced_items: HashSet<CString>,
}

impl Context {
    // Safety: only allowed to be created once
    unsafe fn new() -> Self {
        Self {
            fst_file: None,
            vpi: VPIContext::new(),
            vhpi: VHPIContext::new(),
            current_time: 0,
            trace_items: bumpalo::Bump::new(),
            already_traced_items: HashSet::new(),
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
        let name = CString::new(FST_DUMPFILE).unwrap();
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

        let name = CString::new(FST_DUMPVARS).unwrap();
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

trait FstFileExt {
    fn add_enum(
        &mut self,
        name: CString,
        names: Vec<CString>,
        values: Vec<CString>,
    ) -> anyhow::Result<(HashMap<CString, CString>, u32)>;
}

impl FstFileExt for FstFile {
    fn add_enum(
        &mut self,
        name: CString,
        names: Vec<CString>,
        values: Vec<CString>,
    ) -> anyhow::Result<(HashMap<CString, CString>, u32)> {
        // TODO(robin): add appropriate sized x and z entries in the encoding
        let mut one_hot_values = Vec::new();
        let bits = values.len();
        for i in 0..bits {
            one_hot_values.push(CString::new(format!("{:0width$b}", 1 << i, width = bits))?);
        }
        let enum_handle = self.create_enum_table(
            name,
            names.into_iter().zip(one_hot_values.clone().into_iter()),
        );
        self.emit_enum_table_ref(enum_handle);
        Ok((
            values.into_iter().zip(one_hot_values.into_iter()).collect(),
            bits as _,
        ))
    }
}

fn vhdl_enum_value_to_binstr(enumv: vhpiEnumT) -> CString {
    CString::new(format!("{enumv:b}")).expect("creating CString from String should always work")
}

fn vhdl_logic_vec_to_binstr(enumvs: &[vhpiEnumT]) -> CString {
    CString::new(
        enumvs
            .iter()
            .map(|v| match *v as _ {
                vhpiU => b'U',
                vhpiX => b'X',
                vhpi0 => b'0',
                vhpi1 => b'1',
                vhpiZ => b'Z',
                vhpiW => b'W',
                vhpiL => b'L',
                vhpiH => b'H',
                vhpiDontCare => b'-',
                _ => b' ',
            })
            .collect::<Vec<_>>(),
    )
    .unwrap()
}

fn vhdl_int_val_to_binstr(int: vhpiIntT) -> CString {
    CString::new(format!("{int:032b}")).expect("creating CString from String should always work")
}

impl Context {
    fn dumpfile_compiletf(&mut self) -> anyhow::Result<PLI_INT32> {
        let call_handle = self.vpi.handle(vpiSysTfCall, None)?;
        let mut args_iter = self.vpi.iter(vpiArgument, Some(call_handle))?;
        let potential_filename = args_iter
            .next()
            .ok_or_else(|| anyhow::anyhow!("{FST_DUMPFILE} expects exactly one argument"))?;
        if args_iter.next() == None {
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
            match self.vpi.get(vpiType, Some(depth_arg)) {
                vpiConstant | vpiParameter => match self.vpi.get(vpiConstType, Some(depth_arg)) {
                    vpiBinaryConst | vpiDecConst | vpiIntConst => {
                        // rest of args are things to dump
                        for arg in args_iter {
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
                // log!("dumping {:?}", self.vpi.string.get(vpiFullName, arg));

                self.move_to_scope(arg, |this| this.add_dumpvar_verilog(arg, depth, 0))?;
            }

            // if we did not have any further arguments, dump everything
            if !specific_paths {
                for top_module in self.vpi.iter(vpiInstance, None)? {
                    self.add_dumpvar_verilog(top_module, depth, 0)?;
                }
            }

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
            Some((ty, handle)) => self.with_fst_scope_verilog(ty, handle, |this| {
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

    fn verilog_subtypes_for(ty: PLI_INT32) -> &'static [PLI_INT32] {
        // the ordering here matters
        static FOR_MODULE: &[PLI_INT32] = &[
            vpiInternalScope,
            vpiInterface,
            vpiModule,
            vpiInstance,
            vpiVariables,
            vpiReg,
            vpiNet,
            vpiParameter,
        ];

        static FOR_SCOPE: &[PLI_INT32] = &[
            vpiInternalScope,
            vpiInterface,
            vpiModule,
            vpiVariables,
            vpiReg,
            vpiNet,
            vpiParameter,
        ];

        static FOR_BEGIN: &[PLI_INT32] = &[vpiInternalScope, vpiVariables, vpiReg, vpiParameter];

        match ty {
            vpiModule => FOR_MODULE,
            vpiNamedBegin => FOR_BEGIN,
            vpiGenScope => FOR_SCOPE,
            // TODO(robin): do we want to iterate over modports?
            vpiInterface => {
                static types: &[PLI_INT32] = &[vpiNet, vpiVariables, vpiInterface];
                types
            }
            vpiNetArray => {
                static types: &[PLI_INT32] = &[vpiNet];
                types
            }
            vpiRegArray => {
                static types: &[PLI_INT32] = &[vpiReg];
                types
            }
            vpiPackedArrayVar | vpiPackedArrayNet => {
                static types: &[PLI_INT32] = &[vpiElement];
                types
            }
            vpiStructVar | vpiUnionVar | vpiStructNet => {
                static types: &[PLI_INT32] = &[vpiMember];
                types
            }
            _ => panic!("dont know how to iterate {}", vpi_const_to_str(ty)),
        }
    }

    fn vhdl_subtypes_for(ty: vhpiClassKindT) -> &'static [vhpiOneToManyT] {
        use vhpiClassKindT::*;
        use vhpiOneToManyT::*;

        match ty {
            vhpiRootInstK | vhpiCompInstStmtK => {
                static types: &[vhpiOneToManyT] = &[
                    vhpiInternalRegions,
                    vhpiSigDecls,
                    vhpiVarDecls,
                    vhpiPortDecls,
                    vhpiGenericDecls,
                    vhpiConstDecls,
                    vhpiCompInstStmts,
                    vhpiBlockStmts,
                ];
                types
            }
            vhpiGenericDeclK | vhpiSigDeclK | vhpiSelectedNameK | vhpiIndexedNameK
            | vhpiPortDeclK => {
                static types: &[vhpiOneToManyT] = &[vhpiIndexedNames, vhpiSelectedNames];
                types
            }
            vhpiForGenerateK | vhpiIfGenerateK | vhpiBlockStmtK => {
                static types: &[vhpiOneToManyT] = &[
                    vhpiDecls,
                    vhpiInternalRegions,
                    vhpiSigDecls,
                    vhpiVarDecls,
                    vhpiConstDecls,
                    vhpiCompInstStmts,
                    vhpiBlockStmts,
                ];
                types
            }
            vhpiConstDeclK => {
                static types: &[vhpiOneToManyT] =
                    &[vhpiAttrSpecs, vhpiIndexedNames, vhpiSelectedNames];
                types
            }
            _ => panic!("dont know how to iterate {ty:?}"),
        }
    }

    fn vhdl_is_low_level(&mut self, thing: vhpiHandleT) -> Option<bool> {
        let name = self
            .vhpi
            .string
            .get(vhpiStrPropertyT::vhpiNameP, thing)
            .ok()?;
        if ["BIT", "STD_ULOGIC", "STD_LOGIC"].contains(&name.to_str().ok()?) {
            if ["BIT"].contains(&name.to_str().ok()?) {
                log!("found bit variable, probably cant dump");
            }
            Some(true)
        } else {
            None
        }
    }

    fn vhdl_is_string(&mut self, thing: vhpiHandleT) -> Option<bool> {
        let name = self
            .vhpi
            .string
            .get(vhpiStrPropertyT::vhpiNameP, thing)
            .ok()?;
        if ["CHARACTER"].contains(&name.to_str().ok()?) {
            Some(true)
        } else {
            None
        }
    }

    fn vhdl_is_basic(&mut self, thing: vhpiHandleT) -> Option<bool> {
        self.vhdl_is_low_level(thing)
    }

    fn add_dumpvar_vhdl(
        &mut self,
        thing: vhpiHandleT,
        max_depth: i32,
        current_depth: i32,
        is_const: bool,
    ) -> anyhow::Result<()> {
        if self.vpi.get(vpiLanguage, Some(thing)) == vpiVerilog {
            return self.add_dumpvar_verilog(thing, max_depth, current_depth);
        }

        let base_type = match self.vhpi.get_kind(thing)? {
            vhpiClassKindT::vhpiCompInstStmtK
            | vhpiClassKindT::vhpiProcessStmtK
            | vhpiClassKindT::vhpiAssertStmtK
            | vhpiClassKindT::vhpiIfGenerateK
            | vhpiClassKindT::vhpiSimpleSigAssignStmtK
            | vhpiClassKindT::vhpiCondSigAssignStmtK => None,
            vhpiClassKindT::vhpiSelectedNameK | vhpiClassKindT::vhpiIndexedNameK => {
                let sub_type = self.vhpi.handle(vhpiOneToOneT::vhpiSubtype, thing)?;
                Some(self.vhpi.handle(vhpiOneToOneT::vhpiBaseType, sub_type)?)
            }
            _ => Some(self.vhpi.handle(vhpiOneToOneT::vhpiBaseType, thing)?),
        };

        let is_composite = base_type.and_then(|ty| {
            self.vhpi
                .get(vhpiIntPropertyT::vhpiIsCompositeP, ty)
                .ok()
                .map(|v| v == vhpiTrue)
        });

        let base_type_kind = base_type.and_then(|ty| self.vhpi.get_kind(ty).ok());

        let (basic_array, is_string) = base_type_kind
            .and_then(|base_type_kind| {
                if base_type_kind == vhpiClassKindT::vhpiArrayTypeDeclK {
                    let elem_type = self
                        .vhpi
                        .handle(vhpiOneToOneT::vhpiElemSubtype, base_type?)
                        .ok()?;
                    let base_elem_type = self
                        .vhpi
                        .handle(vhpiOneToOneT::vhpiBaseType, elem_type)
                        .ok()?;

                    Some((
                        self.vhdl_is_basic(base_elem_type).unwrap_or(false),
                        self.vhdl_is_string(base_elem_type).unwrap_or(false),
                    ))
                } else {
                    None
                }
            })
            .unwrap_or((false, false));

        let is_basic = if basic_array {
            true
        } else {
            base_type
                .and_then(|v| self.vhdl_is_basic(v))
                .unwrap_or(false)
        };

        // log!(
        //     "looking at {:?}, kind: {:?}, base type kind: {:?}, isCompositeP {:?}, basic_array {:?}, is_string {:?}",
        //     self.vhpi
        //         .string
        //         .get(vhpiStrPropertyT::vhpiFullVlogNameP, thing)?
        //         .to_owned(),
        //     self.vhpi.get_kind(thing),
        //     base_type_kind,
        //     is_composite,
        //     basic_array,
        //     is_string
        // );

        // TODO(robin): remove this is string and try with strings?
        if max_depth == current_depth {
            Ok(())
        } else if (self.vpi.get(vpiType, Some(thing)) == vpiModule)
            && (self.vpi.get(vpiProtected, Some(thing)) == 1)
        {
            log!("not dumping a protected thing: {:?}", thing);
            Ok(())
        } else {
            let ty = self.vhpi.get_kind(thing)?;
            let is_const = matches!(
                ty,
                vhpiClassKindT::vhpiConstDeclK | vhpiClassKindT::vhpiGenericDeclK
            ) || is_const;

            match (
                self.vhpi.get_kind(thing)?,
                is_composite,
                basic_array,
                is_string,
            ) {
                (ty, _, true, _) | (ty, Some(false), _, _) | (ty, _, _, true) => {
                    if let Err(e) = self.add_trace_vhdl(
                        ty,
                        thing,
                        base_type.ok_or_else(|| {
                            anyhow::anyhow!("no base type but wanted to trace {thing:?}")
                        })?,
                        is_const,
                        is_basic,
                        basic_array,
                        is_string,
                    ) {
                        log!("error adding var: {e}");
                    }

                    Ok(())
                }
                (
                    ty @ (vhpiClassKindT::vhpiCompInstStmtK
                    | vhpiClassKindT::vhpiSigDeclK
                    | vhpiClassKindT::vhpiPortDeclK
                    | vhpiClassKindT::vhpiSelectedNameK
                    | vhpiClassKindT::vhpiIndexedNameK
                    | vhpiClassKindT::vhpiIfGenerateK
                    | vhpiClassKindT::vhpiConstDeclK
                    | vhpiClassKindT::vhpiGenericDeclK),
                    _,
                    _,
                    _,
                ) => self.with_fst_scope_vhdl(ty, base_type, thing, |this| {
                    let next_depth = if ty == vhpiClassKindT::vhpiCompInstStmtK {
                        current_depth + 1
                    } else {
                        current_depth
                    };

                    let mut sub_things = Vec::new();
                    for sub_type in Self::vhdl_subtypes_for(ty) {
                        for sub_thing in this.vhpi.maybe_empty_iter(*sub_type, thing) {
                            if !sub_things.iter().any(|other_thing| {
                                this.vhpi.compare_objects(sub_thing, *other_thing)
                            }) {
                                if let Err(e) = this
                                    .add_dumpvar_vhdl(sub_thing, max_depth, next_depth, is_const)
                                {
                                    log!(
                                        "error adding sub thing {:?} of {:?}: {e:?}",
                                        this.vhpi
                                            .string
                                            .get(vhpiStrPropertyT::vhpiFullVlogNameP, sub_thing)
                                            .map(ToOwned::to_owned),
                                        this.vhpi
                                            .string
                                            .get(vhpiStrPropertyT::vhpiFullVlogNameP, thing)
                                    );
                                } else {
                                    sub_things.push(sub_thing);
                                }
                            }
                        }
                    }

                    Ok(())
                }),
                // TODO(robin): useless?
                (
                    vhpiClassKindT::vhpiCondSigAssignStmtK
                    | vhpiClassKindT::vhpiAssertStmtK
                    | vhpiClassKindT::vhpiSimpleSigAssignStmtK
                    | vhpiClassKindT::vhpiProcessStmtK
                    | vhpiClassKindT::vhpiRecordTypeDeclK,
                    _,
                    _,
                    _,
                ) => Ok(()),
                ty => Err(anyhow::anyhow!(
                    "don't know how to dump {:?} of type {ty:?}",
                    self.vhpi
                        .string
                        .get(vhpiStrPropertyT::vhpiCaseNameP, thing)
                        .map(|v| v.to_owned()),
                )),
            }
        }
    }

    fn add_dumpvar_verilog(
        &mut self,
        thing: vpiHandle,
        max_depth: i32,
        current_depth: i32,
    ) -> anyhow::Result<()> {
        if (max_depth == current_depth) || (self.vpi.get(vpiIsProtected, Some(thing)) == 1) {
            Ok(())
        } else if self.vpi.get(vpiLanguage, Some(thing)) == vpiVHDL {
            self.add_dumpvar_vhdl(thing, max_depth, current_depth, false)
        } else {
            match self.vpi.get(vpiType, Some(thing)) {
                // a leaf value
                ty @ (vpiNet | vpiParameter | vpiLogicVar | vpiRealVar | vpiIntegerVar
                | vpiTimeVar | vpiEnumVar | vpiEnumNet) => {
                    if let Err(e) = self.add_trace_verilog(ty, thing) {
                        log!("error adding var: {e}");
                    }

                    Ok(())
                }
                // something we can recurse on
                ty @ (vpiModule | vpiStructVar | vpiUnionVar | vpiStructNet | vpiInterface
                | vpiRegArray | vpiNamedBegin | vpiGenScope | vpiPackedArrayVar
                | vpiPackedArrayNet) => self.with_fst_scope_verilog(ty, thing, |this| {
                    let next_depth = if ty == vpiModule {
                        current_depth + 1
                    } else {
                        current_depth
                    };

                    let mut sub_things = Vec::new();
                    for sub_type in Self::verilog_subtypes_for(ty) {
                        for sub_thing in this.vpi.maybe_empty_iter(*sub_type, Some(thing)) {
                            if !sub_things.iter().any(|other_thing| {
                                this.vpi.compare_objects(sub_thing, *other_thing)
                            }) {
                                if let Err(e) =
                                    this.add_dumpvar_verilog(sub_thing, max_depth, next_depth)
                                {
                                    log!(
                                        "error adding sub thing {sub_thing:?} of {:?}: {e:?}",
                                        this.vpi.string.get(vpiFullName, thing)
                                    );
                                } else {
                                    sub_things.push(sub_thing);
                                }
                            }
                        }
                    }

                    Ok(())
                }),
                // TODO(robin): add consts for 3rd party simulator stuff
                vpiPackage | vpiFunction | vpiTask | vpiIntVar | vpiTypeParameter => Ok(()),
                ty => Err(anyhow::anyhow!(
                    "don't know how to dump {:?} of type {ty} = {:?}",
                    self.vpi.string.get(vpiName, thing).map(|v| v.to_owned()),
                    self.vpi.string.get(vpiType, thing)
                )),
            }
        }
    }

    fn with_fst_scope_verilog<T>(
        &mut self,
        ty: PLI_INT32,
        handle: vpiHandle,
        func: impl FnOnce(&mut Self) -> anyhow::Result<T>,
    ) -> anyhow::Result<T> {
        let fst_file = self.fst_file.as_mut().unwrap();
        let scope_type = match ty {
            vpiModule => Ok(fstScopeType::FST_ST_VCD_MODULE),
            vpiInterface => Ok(fstScopeType::FST_ST_VCD_INTERFACE),
            vpiStructVar | vpiStructNet => Ok(fstScopeType::FST_ST_VCD_STRUCT),
            // TODO(robin): better fit for array scope type?
            vpiRegArray => Ok(fstScopeType::FST_ST_VCD_MODULE),
            vpiPackedArrayNet | vpiPackedArrayVar => Ok(fstScopeType::FST_ST_VCD_MODULE),
            vpiNamedBegin => Ok(fstScopeType::FST_ST_VCD_BEGIN),
            vpiGenScope => Ok(fstScopeType::FST_ST_VCD_GENERATE),
            _ => Err(anyhow::anyhow!(
                "cannot map vpi type {} to fst scope type",
                vpi_const_to_str(ty)
            )),
        }?;
        let name = self.vpi.string.get(vpiName, handle)?.to_owned();

        let unk = CString::new("unk")?;
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

    fn with_fst_scope_vhdl<T>(
        &mut self,
        ty: vhpiClassKindT,
        base_type: Option<vhpiHandleT>,
        handle: vhpiHandleT,
        func: impl FnOnce(&mut Self) -> anyhow::Result<T>,
    ) -> anyhow::Result<T> {
        let scope_type = match ty {
            vhpiClassKindT::vhpiCompInstStmtK => Ok(fstScopeType::FST_ST_VHDL_ARCHITECTURE),
            vhpiClassKindT::vhpiProcessStmtK => Ok(fstScopeType::FST_ST_VHDL_PROCESS),
            vhpiClassKindT::vhpiIfGenerateK => Ok(fstScopeType::FST_ST_VHDL_IF_GENERATE),
            vhpiClassKindT::vhpiIndexedNameK => Ok(fstScopeType::FST_ST_VCD_SCOPE),
            vhpiClassKindT::vhpiPortDeclK
            | vhpiClassKindT::vhpiSigDeclK
            | vhpiClassKindT::vhpiRecordTypeDeclK
            | vhpiClassKindT::vhpiConstDeclK
            | vhpiClassKindT::vhpiSelectedNameK => match self
                .vhpi
                .get_kind(base_type.expect("signal should have base type"))?
            {
                vhpiClassKindT::vhpiArrayTypeDeclK => Ok(fstScopeType::FST_ST_VHDL_BLOCK),
                vhpiClassKindT::vhpiRecordTypeDeclK => Ok(fstScopeType::FST_ST_VHDL_RECORD),
                ty => Err(anyhow::anyhow!(
                    "cannot map sig vhpi type {:?} to fst scope type",
                    ty
                )),
            },
            _ => Err(anyhow::anyhow!(
                "cannot map vhpi type {:?} to fst scope type",
                ty
            )),
        }?;
        let name = self.vhdl_remove_prefix(ty, handle)?;

        let comp_name = Some(match base_type {
            Some(ty) => self.vhpi.string.get(vhpiStrPropertyT::vhpiCaseNameP, ty)?,
            None => self
                .vhpi
                .string
                .get(vhpiStrPropertyT::vhpiDefNameP, handle)?,
        });

        let fst_file = self.fst_file.as_mut().unwrap();
        fst_file.set_scope(scope_type, name, comp_name);
        let ret = func(self);

        let fst_file = self.fst_file.as_mut().unwrap();
        fst_file.set_upscope();

        ret
    }

    fn add_trace_verilog(&mut self, ty: PLI_INT32, handle: vpiHandle) -> anyhow::Result<()> {
        // log!("dumping {:?}", self.vpi.string.get(vpiFullName, handle)?);
        let full_name = self.vpi.string.get(vpiFullName, handle)?;
        if self.already_traced_items.contains(full_name) {
            Ok(())
        } else {
            self.already_traced_items.insert(full_name.to_owned());
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
            let (encoding, bits): (Option<HashMap<_, _>>, _) =
                if ty == vpiEnumVar || ty == vpiEnumNet {
                    let typespec = self.vpi.handle(vpiTypespec, Some(handle))?;
                    let mut names = Vec::new();
                    let mut values = Vec::new();

                    let anon = CString::new("anonymous")?;
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

                    let (encoding, bits) = fst_file.add_enum(name, names, values)?;

                    (Some(encoding), bits)
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
                    .map(<CString as Deref>::deref)
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
                    fst_handle,
                    encoding,
                    is_string: false,
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
                    cb_rtn: Some(verilog_value_changed_handler),
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

    fn vhdl_remove_prefix(
        &mut self,
        ty: vhpiClassKindT,
        handle: vhpiHandleT,
    ) -> anyhow::Result<CString> {
        let name = self
            .vhpi
            .string
            .get(vhpiStrPropertyT::vhpiCaseNameP, handle)?
            .to_owned();
        if ty == vhpiClassKindT::vhpiSelectedNameK {
            let prefix = self.vhpi.handle(vhpiOneToOneT::vhpiPrefix, handle)?;
            let prefix = self
                .vhpi
                .string
                .get(vhpiStrPropertyT::vhpiCaseNameP, prefix)?;
            Ok(CString::new(
                name.to_str()?
                    .strip_prefix(prefix.to_str()?)
                    .expect("vhdl prefixed name is prefixed by its prefix")
                    .trim_start_matches('.'),
            )?)
        } else {
            Ok(name)
        }
    }

    fn add_trace_vhdl(
        &mut self,
        ty: vhpiClassKindT,
        handle: vhpiHandleT,
        base_type: vhpiHandleT,
        is_const: bool,
        is_basic: bool,
        is_array: bool,
        is_string: bool,
    ) -> anyhow::Result<()> {
        let full_name = self
            .vhpi
            .string
            .get(vhpiStrPropertyT::vhpiFullVlogNameP, handle)?;

        if self.already_traced_items.contains(full_name) {
            Ok(())
        } else {
            let full_name = full_name.to_owned();
            let base_type_kind = self.vhpi.get_kind(base_type)?;

            // log!(
            //     "tracing {:?}, name: {:?}, base type: {:?}",
            //     full_name,
            //     self.vhpi
            //         .string
            //         .get(vhpiStrPropertyT::vhpiNameP, handle)?
            //         .to_owned(),
            //     base_type_kind
            // );

            self.already_traced_items.insert(full_name);
            let is_complicated_enum =
                matches!(base_type_kind, vhpiClassKindT::vhpiEnumTypeDeclK) && !is_basic;

            let var_type = if is_string {
                fstVarType::FST_VT_GEN_STRING
            } else if is_complicated_enum {
                fstVarType::FST_VT_SV_ENUM
            } else if is_const {
                fstVarType::FST_VT_VCD_PARAMETER
            } else if base_type_kind == vhpiClassKindT::vhpiIntTypeDeclK {
                fstVarType::FST_VT_VCD_INTEGER
            } else {
                fstVarType::FST_VT_VCD_WIRE
            };

            let var_name = self.vhdl_remove_prefix(ty, handle)?;
            let fst_file = self.fst_file.as_mut().unwrap();

            // TODO(robin): store these somewhere and then reuse?
            let (encoding, bits): (Option<HashMap<_, _>>, _) = if is_complicated_enum {
                let mut names = Vec::new();
                let mut values = Vec::new();

                let name = self
                    .vhpi
                    .string
                    .get(vhpiStrPropertyT::vhpiCaseNameP, base_type)?
                    .to_owned();

                for enum_lit in self
                    .vhpi
                    .iter(vhpiOneToManyT::vhpiEnumLiterals, base_type)?
                {
                    let name = self
                        .vhpi
                        .string
                        .get(vhpiStrPropertyT::vhpiStrValP, enum_lit)?
                        .to_owned();
                    let value = self.vhpi.get(vhpiIntPropertyT::vhpiPositionP, enum_lit)?;

                    names.push(name);
                    values.push(vhdl_enum_value_to_binstr(value as _));
                }

                let (encoding, bits) = fst_file.add_enum(name, names, values)?;

                (Some(encoding), bits)
            } else {
                let bits = match base_type_kind {
                    vhpiClassKindT::vhpiIntTypeDeclK => 32, // TODO(robin): evaluate constraints
                    _ => self.vhpi.get(vhpiIntPropertyT::vhpiSizeP, handle)? as u32,
                };
                (None, bits)
            };

            let fst_handle =
                fst_file.create_var(var_type, fstVarDir::FST_VD_IMPLICIT, bits, var_name, None);

            let value_format = if is_string {
                vhpiFormatT::vhpiStrVal
            } else if is_complicated_enum {
                vhpiFormatT::vhpiEnumVal
            } else if is_basic && is_array {
                vhpiFormatT::vhpiLogicVecVal
            } else if is_basic {
                vhpiFormatT::vhpiLogicVal
            } else if base_type_kind == vhpiClassKindT::vhpiIntTypeDeclK {
                vhpiFormatT::vhpiIntVal
            } else {
                vhpiFormatT::vhpiBinStrVal
            };

            let value = match self.vhpi.get_value(
                value_format,
                if is_string { bits + 1 } else { bits },
                handle,
            ) {
                Ok(vhpi::Value::Str(v)) => v,
                Ok(vhpi::Value::BinStr(v)) => v,
                Ok(vhpi::Value::LogicVecVal(v)) => vhdl_logic_vec_to_binstr(&v),
                Ok(vhpi::Value::LogicVal(v)) => vhdl_logic_vec_to_binstr(&[v]),
                Ok(vhpi::Value::IntVal(v)) => vhdl_int_val_to_binstr(v),
                Ok(vhpi::Value::Enum(v)) => {
                    let encoded = vhdl_enum_value_to_binstr(v);
                    encoding
                        .as_ref()
                        .and_then(|e| e.get(&encoded))
                        .map(|v| v.to_owned())
                        .unwrap_or(encoded)
                }
                Err(_) => {
                    return Err(anyhow::anyhow!(
                        "got no value for {:?}",
                        self.vhpi
                            .string
                            .get(vhpiStrPropertyT::vhpiFullVlogNameP, handle)?
                    ))
                }
            };

            if is_string {
                self.fst_file
                    .as_mut()
                    .unwrap()
                    .emit_var_length_value_change(fst_handle, value);
            } else {
                self.fst_file
                    .as_mut()
                    .unwrap()
                    .emit_value_change(fst_handle, value);
            }

            if !is_const {
                let cheated_context_ref = unsafe { &mut *(self as *mut Context) };
                let trace_item = self.trace_items.alloc(TraceItem {
                    context: cheated_context_ref,
                    fst_handle,
                    encoding,
                    is_string,
                });

                let mut time_config: vhpiTimeT = Default::default();
                let mut value_config = vhpiValueS {
                    format: value_format,
                    bufSize: bits,
                    value: vhpiValueS__bindgen_ty_1 {
                        str_: unsafe {
                            CString::from_vec_unchecked(vec![0; bits as usize]).into_raw()
                        },
                    },
                    ..Default::default()
                };

                let mut value_change_cb = vhpiCbDataT {
                    reason: vhpiCbValueChange,
                    cb_rtn: Some(vhdl_value_changed_handler),
                    obj: handle,
                    user_data: trace_item as *mut _ as *mut PLI_VOID,
                    time: &mut time_config as *mut _,
                    value: &mut value_config as *mut _,
                };

                unsafe { vhpi_register_cb(&mut value_change_cb as _, 0) };
            }

            Ok(())
        }
    }
}

unsafe extern "C" fn verilog_value_changed_handler(data: *mut t_cb_data) -> PLI_INT32 {
    let trace_item = &mut *((*data).user_data as *mut TraceItem);
    catch_error(|| {
        trace_item.trip(
            Time::from(*(*data).time),
            Value::try_from(*(*data).value).unwrap(),
        )
    })
}

unsafe extern "C" fn vhdl_value_changed_handler(data: *const vhpiCbDataS) -> PLI_VOID {
    catch_error(|| {
        let enum_value = match (*(*data).value).format {
            vhpiFormatT::vhpiEnumVal => {
                Some(vhdl_enum_value_to_binstr((*(*data).value).value.enumv))
            }
            vhpiFormatT::vhpiIntVal => Some(vhdl_int_val_to_binstr((*(*data).value).value.intg)),
            vhpiFormatT::vhpiLogicVal => {
                Some(vhdl_logic_vec_to_binstr(&[(*(*data).value).value.enumv]))
            }
            vhpiFormatT::vhpiLogicVecVal => {
                Some(vhdl_logic_vec_to_binstr(std::slice::from_raw_parts(
                    (*(*data).value).value.enumvs,
                    (*(*data).value).bufSize as _,
                )))
            }
            _ => None,
        };
        let value = Value::BinStr(
            enum_value
                .as_deref()
                .unwrap_or_else(|| CStr::from_ptr((*(*data).value).value.str_)),
        );
        let trace_item = &mut *((*data).user_data as *mut TraceItem);
        trace_item.trip(
            Time::Sim((((*(*data).time).high as u64) << 32 | ((*(*data).time).low as u64)) / 1000),
            value,
        )
    });
    std::mem::transmute(0u8)
}

unsafe fn alloc_inited<T>(init_value: T) -> *mut T {
    let ptr = std::alloc::alloc(std::alloc::Layout::new::<T>());
    // use ptr::write to avoid drop being called on the uninitialized T being overwritten
    std::ptr::write(ptr as *mut T, init_value);

    ptr as *mut T
}

// TODO(robin):
// time vars
// real vars
// unwrap cleanup
// strings
