package com.pcalouche.spat.security;

import com.pcalouche.spat.model.SpatUser;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.Collection;

public class SpatUserDetails implements UserDetails {
    private String username;
    private String password;
    private String role;

    //    public static List<GrantedAuthority> createAuthorityList(String... roles) {
    //        List<GrantedAuthority> authorities = new ArrayList<>(roles.length);
    //
    //        for (String role : roles) {
    //            authorities.add(new SimpleGrantedAuthority(role));
    //        }
    //
    //        return authorities;
    //    }

    public SpatUserDetails(SpatUser spatUser) {
        this.username = spatUser.getUsername();
        this.password = spatUser.getPassword();
        this.role = spatUser.getRole();
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return AuthorityUtils.commaSeparatedStringToAuthorityList(role);
    }

    @Override
    public String getUsername() {
        return null;
    }

    @Override
    public String getPassword() {
        return null;
    }

    @Override
    public boolean isAccountNonExpired() {
        return false;
    }

    @Override
    public boolean isAccountNonLocked() {
        return false;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return false;
    }

    @Override
    public boolean isEnabled() {
        return false;
    }
}
