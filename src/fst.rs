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

    // for strings
    pub fn emit_var_length_value_change(
        &mut self,
        handle: fst_sys::fstHandle,
        value: impl std::ops::Deref<Target = std::ffi::CStr>,
    ) {
        unsafe {
            fst_sys::fstWriterEmitVariableLengthValueChange(
                self.file,
                handle,
                value.as_ptr() as _,
                value.to_bytes().len() as _,
            )
        }
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

    pub fn create_enum_table(
        &mut self,
        name: impl std::ops::Deref<Target = std::ffi::CStr>,
        name_value_pairs: impl Iterator<Item = (CString, CString)>,
    ) -> fst_sys::fstEnumHandle {
        let mut names_holder = Vec::new();
        let mut values_holder = Vec::new();
        let mut names = Vec::new();
        let mut values = Vec::new();
        for (name, value) in name_value_pairs {
            names_holder.push(name);
            values_holder.push(value);
            names.push(names_holder.last().unwrap().as_ptr());
            values.push(values_holder.last().unwrap().as_ptr());
        }

        unsafe {
            fst_sys::fstWriterCreateEnumTable(
                self.file,
                name.as_ptr(),
                names.len() as u32,
                0, /* min val bits */
                names.as_mut_ptr(),
                values.as_mut_ptr(),
            )
        }
    }

    pub fn emit_enum_table_ref(&mut self, enum_handle: fst_sys::fstEnumHandle) {
        unsafe { fst_sys::fstWriterEmitEnumTableRef(self.file, enum_handle) }
    }
}

impl Drop for FstFile {
    fn drop(&mut self) {
        unsafe { fst_sys::fstWriterClose(self.file) }
    }
}
