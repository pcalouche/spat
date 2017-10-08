package com.pcalouche.spat.controller.user;

import com.pcalouche.spat.controller.AbstractController;
import com.pcalouche.spat.model.User;
import com.pcalouche.spat.service.user.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = UserUris.ROOT)
public class UserController extends AbstractController {
    private final UserService userService;

    @Autowired
    public UserController(UserService userService) {
        this.userService = userService;
    }

    @RequestMapping(method = RequestMethod.GET)
    public List<User> getUsers() {
        return userService.getUsers();
    }

    @RequestMapping(method = RequestMethod.POST)
    public User saveUser(@RequestBody User user) {
        logger.debug("User to save name is " + user.getFullName() + " " + user.getId());
        return userService.saveUser(user);
    }

    @RequestMapping(value = "/{id}", method = RequestMethod.DELETE)
    public ResponseEntity<Boolean> deleteUser(@PathVariable Long id) {
        logger.info("ID to delete from controller is " + id);
        return new ResponseEntity<>(userService.deleteUser(id), HttpStatus.OK);
    }
}
