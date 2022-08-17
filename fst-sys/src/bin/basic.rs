fn main() {
    unsafe {
        let res =
            fst_sys::fstWriterCreate(std::ffi::CString::new("hello.fst").unwrap().as_ptr(), 1);

        fst_sys::fstWriterClose(res);
    }
}
