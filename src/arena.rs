use std::collections::HashMap;

use crate::vm::{Object, SynString, Classifier};

pub type Pointer = usize;
pub type Word = Classifier<Object>;

pub struct Arena {
    strings: HashMap<String, SynString>,
    data: Vec<Word>,
}

impl Arena {
    pub fn new() -> Arena {
        Arena {
            strings: HashMap::new(),
            data: Vec::new()
        }
    }

    pub fn get_obj(&self, ptr: Pointer) -> Option<&Word> {
        if self.data.len() <= ptr {
            return None;
        }
        return Some(&self.data[ptr])
    }

    pub fn get_objs(&self, ptr: Pointer, len: usize) -> Option<impl Iterator<Item = Word>> {
        if ptr + len > self.data.len() {
            return None;
        }
        Some(self.data[ptr..ptr + len].iter().cloned())
    }

    pub fn get_obj_mut(&mut self, ptr: Pointer) -> Option<&mut Word> {
        if self.data.len() <= ptr {
            return None;
        }
        return Some(&mut self.data[ptr]);
    }

    #[allow(dead_code)]
    pub fn get_objs_mut(&mut self, ptr: Pointer, len: usize) -> Option<&mut [Classifier<Object>]> {
        if self.data.len() <= ptr + len {
            return None;
        }
        return Some(&mut self.data[ptr..ptr+len]);
    }

    pub fn push_obj(&mut self, obj: Classifier<Object>) -> Pointer {
        let ptr = self.data.len();
        self.data.push(obj);
        return ptr;
    }

    pub fn push_objs(&mut self, objs: Vec<Classifier<Object>>) -> Pointer {
        let ptr = self.data.len();
        objs.iter().for_each(|obj| self.data.push(obj.clone()));
        return ptr;
    }
}

impl Arena {
    pub fn str_to_synstr(&mut self, src: String) -> SynString {
        if let Some(synstr) = self.strings.get(&src) {
            return synstr.clone();
        }
        let mut vec = Vec::with_capacity(src.len() + 1);
        for b in src.bytes() {
            vec.push(Classifier::Runtime(Object::Byte(b)));
        }
        vec.push(Classifier::Runtime(Object::Byte(0)));
        let synstr = SynString { base: self.push_objs(vec), len: src.len() + 1 };
        self.strings.insert(src, synstr.clone());
        return synstr;
    }
}
