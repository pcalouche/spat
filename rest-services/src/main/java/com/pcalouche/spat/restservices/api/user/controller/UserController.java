package com.pcalouche.spat.restservices.api.user.controller;

import com.pcalouche.spat.restservices.api.AbstractSpatController;
import com.pcalouche.spat.restservices.api.model.User;
import com.pcalouche.spat.restservices.api.user.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = UserEndpoints.ROOT)
public class UserController extends AbstractSpatController {
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
        logger.debug("User to save name is " + user.getUsername() + " " + user.getId());
        return userService.saveUser(user);
    }

    @DeleteMapping(value = "/{id}")
    public ResponseEntity<Boolean> deleteUser(@PathVariable Long id) {
        logger.debug("ID to delete from controller is " + id);
        return new ResponseEntity<>(userService.deleteUser(id), HttpStatus.OK);
    }
}
