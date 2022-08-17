use std::ffi::CStr;

use vpi_sys::{
    t_vpi_strengthval, t_vpi_time, t_vpi_value, t_vpi_vecval, vpiBinStrVal, vpiDecStrVal,
    vpiHandle, vpiHexStrVal, vpiIntVal, vpiObjTypeVal, vpiOctStrVal, vpiRealVal, vpiScalarVal,
    vpiScaledRealTime, vpiSimTime, vpiStrengthVal, vpiStringVal, vpiTimeVal, vpiVectorVal,
    vpi_compare_objects, vpi_free_object, vpi_get, vpi_get_str, vpi_get_time, vpi_get_value,
    vpi_handle, vpi_iterate, vpi_scan, PLI_INT32,
};

type Result<T> = std::result::Result<T, VPIError>;

// TODO(robin): split these up into enums (for example a enum with all string properties, etc)
type VPITy = i32;

#[derive(Debug)]
pub struct VPIString(());

impl VPIString {
    pub fn get<'a>(&'a mut self, ty: VPITy, object: vpiHandle) -> Result<&'a CStr> {
        unsafe {
            let ptr = vpi_get_str(ty, object);
            if ptr.is_null() {
                Err(VPIError::NullString(ty, object))
            } else {
                Ok(CStr::from_ptr(ptr))
            }
        }
    }
}

#[derive(Debug)]
pub struct VPIValue(());

impl VPIValue {
    pub fn get<'a>(&'a mut self, ty: ValueTy, object: vpiHandle) -> Value<'a> {
        let mut value = t_vpi_value {
            format: ty as _,
            ..Default::default()
        };
        unsafe {
            vpi_get_value(object, &mut value as *mut _);
        }

        value.into()
    }
}

#[derive(num_enum::TryFromPrimitive)]
#[repr(i32)]
pub enum ValueTy {
    BinStr = vpiBinStrVal,
    OctStr = vpiOctStrVal,
    HexStr = vpiHexStrVal,
    DecStr = vpiDecStrVal,
    Scalar = vpiScalarVal,
    Int = vpiIntVal,
    Real = vpiRealVal,
    String = vpiStringVal,
    Time = vpiTimeVal,
    Vector = vpiVectorVal,
    Strength = vpiStrengthVal,
    ObjType = vpiObjTypeVal,
}

#[derive(Debug)]
pub enum Value<'a> {
    BinStr(&'a CStr),
    OctStr(&'a CStr),
    DecStr(&'a CStr),
    HexStr(&'a CStr),
    Scalar(PLI_INT32),
    Int(PLI_INT32),
    Real(f64),
    String(&'a CStr),
    Vector(&'a t_vpi_vecval),
    Strength(&'a t_vpi_strengthval),
    Time(&'a t_vpi_time),
}

impl<'a> From<t_vpi_value> for Value<'a> {
    fn from(value: t_vpi_value) -> Self {
        unsafe {
            match ValueTy::try_from(value.format).unwrap() {
                ValueTy::BinStr => Value::BinStr(CStr::from_ptr(value.value.str_)),
                ValueTy::OctStr => Value::OctStr(CStr::from_ptr(value.value.str_)),
                ValueTy::DecStr => Value::DecStr(CStr::from_ptr(value.value.str_)),
                ValueTy::HexStr => Value::HexStr(CStr::from_ptr(value.value.str_)),
                ValueTy::Scalar => Value::Scalar(value.value.scalar),
                ValueTy::Int => Value::Int(value.value.integer),
                ValueTy::Real => Value::Real(value.value.real),
                ValueTy::String => Value::String(CStr::from_ptr(value.value.str_)),
                ValueTy::Time => Value::Time(&*value.value.time),
                ValueTy::Vector => Value::Vector(&*value.value.vector),
                ValueTy::Strength => Value::Strength(&*value.value.strength),
                ValueTy::ObjType => unreachable!(),
            }
        }
    }
}

#[derive(num_enum::TryFromPrimitive)]
#[repr(i32)]
pub enum TimeTy {
    Sim = vpiSimTime,
    ScaledReal = vpiScaledRealTime,
}

#[derive(Debug)]
pub enum Time {
    Sim(u64),
    ScaledReal(f64),
}

impl From<t_vpi_time> for Time {
    fn from(value: t_vpi_time) -> Self {
        match TimeTy::try_from(value.type_).unwrap() {
            TimeTy::Sim => Time::Sim((value.high as u64) << 32 | (value.low as u64)),
            TimeTy::ScaledReal => Time::ScaledReal(value.real),
        }
    }
}

#[derive(Debug)]
pub struct VPIContext {
    pub string: VPIString,
    pub value: VPIValue,
}

trait OptionExt<T> {
    fn into_ptr(self) -> *mut T;
}

impl<T> OptionExt<T> for Option<*mut T> {
    fn into_ptr(self) -> *mut T {
        self.unwrap_or(std::ptr::null_mut())
    }
}

trait PtrExt<T> {
    fn from_ptr(self) -> Option<*mut T>;
}

impl<T> PtrExt<T> for *mut T {
    fn from_ptr(self) -> Option<*mut T> {
        if self.is_null() {
            None
        } else {
            Some(self)
        }
    }
}

impl VPIContext {
    // Safety: only allowed to be created once
    pub unsafe fn new() -> Self {
        Self {
            string: VPIString(()),
            value: VPIValue(()),
        }
    }

    pub fn handle(&self, ty: VPITy, reference: Option<vpiHandle>) -> Result<vpiHandle> {
        let handle: Option<vpiHandle> = unsafe { vpi_handle(ty, reference.into_ptr()).from_ptr() };
        handle.ok_or_else(|| VPIError::NullHandle(ty, reference))
    }

    pub fn iter(&self, ty: VPITy, reference: Option<vpiHandle>) -> Result<VPIIterator> {
        let iterator_handle = unsafe { vpi_iterate(ty, reference.into_ptr()).from_ptr() };
        iterator_handle
            .ok_or_else(|| VPIError::NullHandleIterate(ty, reference))
            .map(|iterator_handle| VPIIterator {
                iterator_handle,
                empty: false,
                done: false,
            })
    }

    pub fn maybe_empty_iter(&self, ty: VPITy, reference: Option<vpiHandle>) -> VPIIterator {
        let iterator_handle = unsafe { vpi_iterate(ty, reference.into_ptr()) };
        VPIIterator {
            empty: iterator_handle.is_null(),
            iterator_handle,
            done: false,
        }
    }

    pub fn get(&self, ty: VPITy, reference: Option<vpiHandle>) -> PLI_INT32 {
        unsafe { vpi_get(ty, reference.into_ptr()) }
    }

    pub fn get_time(&self, ty: TimeTy, reference: Option<vpiHandle>) -> Time {
        let mut time = t_vpi_time {
            type_: ty as _,
            ..Default::default()
        };
        unsafe {
            vpi_get_time(reference.into_ptr(), &mut time as *mut _);
        }

        time.into()
    }

    pub fn compare_objects(&self, obj_a: vpiHandle, obj_b: vpiHandle) -> bool {
        unsafe { vpi_compare_objects(obj_a, obj_b) == 1 }
    }
}

pub struct VPIIterator {
    iterator_handle: vpiHandle,
    empty: bool,
    done: bool,
}

impl Iterator for VPIIterator {
    type Item = vpiHandle;

    fn next(&mut self) -> Option<Self::Item> {
        if self.empty {
            None
        } else {
            let item = unsafe { vpi_scan(self.iterator_handle) }.from_ptr();
            self.done = item.is_none();
            item
        }
    }
}

impl Drop for VPIIterator {
    fn drop(&mut self) {
        if !self.empty && !self.done {
            unsafe {
                vpi_free_object(self.iterator_handle);
            }
        }
    }
}

// TODO(robin): better printing for abstract values?
#[derive(thiserror::Error, Debug)]
pub enum VPIError {
    #[error("null handle obtained for type {} on ref {1:?}", vpi_const_to_str(*.0))]
    NullHandle(VPITy, Option<vpiHandle>),
    #[error("null handle obtained iterator for type {} on ref {1:?}", vpi_const_to_str(*.0))]
    NullHandleIterate(VPITy, Option<vpiHandle>),
    #[error("null string obtained for type {} on ref {1:?}", vpi_const_to_str(*.0))]
    NullString(VPITy, vpiHandle),
}

unsafe impl Send for VPIError {}
unsafe impl Sync for VPIError {}

mod vpi_to_str;
pub use vpi_to_str::vpi_const_to_str;
