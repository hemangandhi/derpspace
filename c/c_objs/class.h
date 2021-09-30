#ifndef _CLASS_H_
#define _CLASS_H_

#define CLASS struct

#define DEFINE_CLASS(name, ...) typedef struct name { __VA_ARGS__ } name;

#define ADD_CLASS_METHOD(return_type, typename, name, ...) return_type name(typename* this, __VA_ARGS__)

#define ADD_GETTER_METHOD(return_type, typename, name) return_type name(typename* this)

#define INVOKE_PTR_METHOD(instance, method_name, ...) method_name(instance, __VA_ARGS__)

#define INVOKE_METHOD(instance, method_name, ...) method_name(&instance, __VA_ARGS__)

#define INVOKE_PTR_GETTER_METHOD(instance, method_name, ...) method_name(instance)

#define INVOKE_GETTER_METHOD(instance, method_name, ...) method_name(&instance)

#endif
