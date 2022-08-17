use std::ffi::CString;

#[derive(Debug)]
pub struct FstFile {
    file: *mut std::ffi::c_void,
}

impl FstFile {
    pub fn new(filename: &str, use_compressed_hierarchy: bool) -> Self {
        Self {
            file: unsafe {
                let name = CString::new(filename).unwrap();
                fst_sys::fstWriterCreate(
                    name.as_ptr(),
                    if use_compressed_hierarchy { 1 } else { 0 },
                )
            },
        }
    }

    pub fn set_scope(
        &mut self,
        scope_type: fst_sys::fstScopeType,
        name: impl std::ops::Deref<Target = std::ffi::CStr>,
        comp_name: Option<impl std::ops::Deref<Target = std::ffi::CStr>>,
    ) {
        unsafe {
            fst_sys::fstWriterSetScope(
                self.file,
                scope_type,
                name.as_ptr(),
                comp_name.map(|s| s.as_ptr()).unwrap_or(std::ptr::null()),
            )
        }
    }

    pub fn set_upscope(&mut self) {
        unsafe { fst_sys::fstWriterSetUpscope(self.file) }
    }

    pub fn set_timescale(&mut self, timescale: i32) {
        unsafe { fst_sys::fstWriterSetTimescale(self.file, timescale) }
    }

    pub fn emit_time_change(&mut self, time: u64) {
        unsafe { fst_sys::fstWriterEmitTimeChange(self.file, time) }
    }

    pub fn emit_value_change(
        &mut self,
        handle: fst_sys::fstHandle,
        value: impl std::ops::Deref<Target = std::ffi::CStr>,
    ) {
        unsafe { fst_sys::fstWriterEmitValueChange(self.file, handle, value.as_ptr() as _) }
    }

    pub fn create_var(
        &mut self,
        ty: fst_sys::fstVarType,
        dir: fst_sys::fstVarDir,
        len: u32,
        name: impl std::ops::Deref<Target = std::ffi::CStr>,
        alias_handle: Option<fst_sys::fstHandle>,
    ) -> fst_sys::fstHandle {
        unsafe {
            fst_sys::fstWriterCreateVar(
                self.file,
                ty,
                dir,
                len,
                name.as_ptr(),
                alias_handle.unwrap_or(0),
            )
        }
    }
}

impl Drop for FstFile {
    fn drop(&mut self) {
        unsafe { fst_sys::fstWriterClose(self.file) }
    }
}
