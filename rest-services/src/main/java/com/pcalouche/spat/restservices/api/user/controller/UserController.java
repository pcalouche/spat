package com.pcalouche.spat.restservices.api.user.controller;

import com.pcalouche.spat.restservices.api.model.User;
import com.pcalouche.spat.restservices.api.user.service.UserService;
import com.pcalouche.spat.restservices.util.LoggerUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = UserEndpoints.ROOT)
public class UserController {
    private final UserService userService;

    @Autowired
    public UserController(UserService userService) {
        this.userService = userService;
    }

    @GetMapping
    public List<User> getUsers() {
        return userService.getUsers();
    }

    @PostMapping
    public User saveUser(@RequestBody User user) {
        LoggerUtils.logDebug("User to save name is " + user.getUsername() + " " + user.getId());
        return userService.saveUser(user);
    }

    @DeleteMapping(value = "/{id}")
    public ResponseEntity<Boolean> deleteUser(@PathVariable Long id) {
        LoggerUtils.logDebug("ID to delete from controller is " + id);
        return new ResponseEntity<>(userService.deleteUser(id), HttpStatus.OK);
    }
}
