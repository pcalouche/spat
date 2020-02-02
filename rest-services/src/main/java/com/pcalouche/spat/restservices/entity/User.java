package com.pcalouche.spat.restservices.entity;

import lombok.*;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import javax.persistence.*;
import java.io.Serializable;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

@Entity
@Table(name = "users")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class User implements UserDetails, Serializable {
    @Id
    private String username;
    private String password;
    @Builder.Default
    private boolean accountNonExpired = true;
    @Builder.Default
    private boolean accountNonLocked = true;
    @Builder.Default
    private boolean credentialsNonExpired = true;
    @Builder.Default
    private boolean enabled = true;
    @ManyToMany(fetch = FetchType.EAGER)
    @JoinTable(name = "user_role", joinColumns = @JoinColumn(name = "user_id"), inverseJoinColumns = @JoinColumn(name = "role_id"))
    @Builder.Default
    private Set<Role> roles = new HashSet<>();
    @Transient
    @Setter(AccessLevel.NONE)
    private Set<SimpleGrantedAuthority> authorities;

    @Override
    public Set<SimpleGrantedAuthority> getAuthorities() {
        if (authorities == null) {
            authorities = new HashSet<>();
            if (roles != null) {
                for (Role role : roles) {
                    authorities.add(new SimpleGrantedAuthority(role.getName()));
                }
            }
        }
        return authorities;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof User)) {
            return false;
        }
        User castedObj = (User) o;
        return Objects.equals(username, castedObj.getUsername());
    }

    @Override
    public int hashCode() {
        return Objects.hash(username);
    }
}
