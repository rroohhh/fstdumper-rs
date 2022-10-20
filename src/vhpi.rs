use std::ffi::{CStr, CString};

use vhpi_sys::{
    vhpiClassKindT, vhpiEnumT, vhpiFormatT, vhpiHandleT, vhpiIntPropertyT, vhpiIntT,
    vhpiOneToManyT, vhpiOneToOneT, vhpiStrPropertyT, vhpiTrue, vhpiUndefined, vhpiValueS,
    vhpiValueS__bindgen_ty_1, vhpi_compare_handles, vhpi_get, vhpi_get_str, vhpi_get_value,
    vhpi_handle, vhpi_iterator, vhpi_release_handle, vhpi_scan, PLI_INT32,
};

type Result<T> = std::result::Result<T, VHPIError>;

#[derive(Debug)]
pub struct VHPIString(());

impl VHPIString {
    pub fn get(&mut self, ty: vhpiStrPropertyT, object: vhpiHandleT) -> Result<&CStr> {
        unsafe {
            let ptr = vhpi_get_str(ty, object);
            if ptr.is_null() {
                Err(VHPIError::NullString(ty, object))
            } else {
                Ok(CStr::from_ptr(ptr))
            }
        }
    }
}

#[derive(Debug)]
pub enum Value {
    BinStr(CString),
    LogicVecVal(Vec<vhpiEnumT>),
    LogicVal(vhpiEnumT),
    IntVal(vhpiIntT),
    Enum(vhpiEnumT),
}

impl Value {
    // SAFETY: transfers ownership of buffers in value to resulting value
    unsafe fn from(value: vhpiValueS) -> Result<Self> {
        match value.format {
            vhpiFormatT::vhpiBinStrVal => Ok(Value::BinStr(CString::from_raw(value.value.str_))),
            vhpiFormatT::vhpiEnumVal => Ok(Value::Enum(value.value.enumv)),
            vhpiFormatT::vhpiIntVal => Ok(Value::IntVal(value.value.intg)),
            vhpiFormatT::vhpiLogicVal => Ok(Value::LogicVal(value.value.enumv)),
            vhpiFormatT::vhpiLogicVecVal => Ok(Value::LogicVecVal(Vec::from_raw_parts(
                value.value.enumvs,
                value.bufSize as _,
                value.bufSize as _,
            ))),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct VHPIContext {
    pub string: VHPIString,
    pub resolution_limit: u64,
}

impl VHPIContext {
    // Safety: only allowed to be created once
    pub unsafe fn new() -> Self {
        Self {
            string: VHPIString(()),
            resolution_limit: 0,
        }
    }

    pub fn get(&self, ty: vhpiIntPropertyT, reference: vhpiHandleT) -> Result<PLI_INT32> {
        let ret = unsafe { vhpi_get(ty, reference) };
        if ret == vhpiUndefined {
            Err(VHPIError::UndefinedIntProperty(ty, reference))
        } else {
            Ok(ret)
        }
    }

    pub fn handle(&self, ty: vhpiOneToOneT, reference: vhpiHandleT) -> Result<vhpiHandleT> {
        let ptr = unsafe { vhpi_handle(ty, reference) };
        if ptr.is_null() {
            Err(VHPIError::NullHandle(ty, reference))
        } else {
            Ok(ptr)
        }
    }

    pub fn get_kind(&self, reference: vhpiHandleT) -> Result<vhpiClassKindT> {
        // TODO(robin): this should really be done better
        self.get(vhpiIntPropertyT::vhpiKindP, reference)
            .map(|v| unsafe { std::mem::transmute(v) })
    }

    pub fn get_value(
        &self,
        ty: vhpiFormatT,
        buf_size: u32,
        reference: vhpiHandleT,
    ) -> Result<Value> {
        let (value, buf_size) = match ty {
            vhpiFormatT::vhpiEnumVal | vhpiFormatT::vhpiIntVal | vhpiFormatT::vhpiLogicVal => {
                (Default::default(), 0)
            }
            vhpiFormatT::vhpiLogicVecVal => {
                let mut buf = Vec::with_capacity(buf_size as _);
                let ptr = buf.as_mut_ptr();
                // Vec::into_raw_parts is unstable, so use as_mut_ptr and forget the Vec here to it does not get deallocated
                std::mem::forget(buf);
                (vhpiValueS__bindgen_ty_1 { enumvs: ptr }, buf_size)
            }
            vhpiFormatT::vhpiBinStrVal => (
                vhpiValueS__bindgen_ty_1 {
                    str_: unsafe {
                        CString::from_vec_unchecked(vec![0; buf_size as usize]).into_raw()
                    },
                },
                buf_size,
            ),
            _ => unimplemented!(),
        };
        let mut value = vhpiValueS {
            format: ty as _,
            bufSize: buf_size,
            value,
            ..Default::default()
        };
        unsafe {
            vhpi_get_value(reference, &mut value as *mut _);
            Value::from(value)
        }
    }

    pub fn iter(&self, ty: vhpiOneToManyT, reference: vhpiHandleT) -> Result<VHPIIterator> {
        let iterator_handle = unsafe { vhpi_iterator(ty, reference) };
        if iterator_handle.is_null() {
            Err(VHPIError::NullHandleIterate(ty, reference))
        } else {
            Ok(VHPIIterator {
                iterator_handle,
                empty: false,
                done: false,
            })
        }
    }

    pub fn maybe_empty_iter(&self, ty: vhpiOneToManyT, reference: vhpiHandleT) -> VHPIIterator {
        let iterator_handle = unsafe { vhpi_iterator(ty, reference) };
        VHPIIterator {
            empty: iterator_handle.is_null(),
            iterator_handle,
            done: false,
        }
    }

    pub fn compare_objects(&self, a: vhpiHandleT, b: vhpiHandleT) -> bool {
        unsafe { vhpi_compare_handles(a, b) == vhpiTrue }
    }
}

pub struct VHPIIterator {
    iterator_handle: vhpiHandleT,
    empty: bool,
    done: bool,
}

impl Iterator for VHPIIterator {
    type Item = vhpiHandleT;

    fn next(&mut self) -> Option<Self::Item> {
        if self.empty {
            None
        } else {
            let item = unsafe { vhpi_scan(self.iterator_handle) };
            let item = if item.is_null() { None } else { Some(item) };
            self.done = item.is_none();
            item
        }
    }
}

impl Drop for VHPIIterator {
    fn drop(&mut self) {
        if !self.empty && !self.done {
            unsafe {
                vhpi_release_handle(self.iterator_handle);
            }
        }
    }
}

// TODO(robin): better printing for abstract values?
#[derive(thiserror::Error, Debug)]
pub enum VHPIError {
    #[error("null string obtained for type {0:?} on handle {0:?}")]
    NullString(vhpiStrPropertyT, vhpiHandleT),
    #[error("null iterator obtained for type {0:?} on handle {0:?}")]
    NullHandleIterate(vhpiOneToManyT, vhpiHandleT),
    #[error("null handle obtained for type {0:?} on handle {0:?}")]
    NullHandle(vhpiOneToOneT, vhpiHandleT),
    #[error("int property {0:?} undefined on handle {0:?}")]
    UndefinedIntProperty(vhpiIntPropertyT, vhpiHandleT),
}

unsafe impl Send for VHPIError {}
unsafe impl Sync for VHPIError {}
