package com.pcalouche.spat.restservices.api.dto;

import com.pcalouche.spat.restservices.api.entity.User;
import org.modelmapper.AbstractConverter;
import org.modelmapper.Converter;
import org.modelmapper.ModelMapper;
import org.modelmapper.PropertyMap;
import org.springframework.security.core.authority.SimpleGrantedAuthority;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class UserDto implements EntityConvertible<User> {
    private Long id;
    private String username;
    private boolean accountNonExpired = true;
    private boolean accountNonLocked = true;
    private boolean credentialsNonExpired = true;
    private boolean enabled = true;
    private List<String> authorities;

    public UserDto() {
    }

    public UserDto(Long id, String username, List<String> authorities) {
        this.id = id;
        this.username = username;
        this.authorities = authorities;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public boolean isAccountNonExpired() {
        return accountNonExpired;
    }

    public void setAccountNonExpired(boolean accountNonExpired) {
        this.accountNonExpired = accountNonExpired;
    }

    public boolean isAccountNonLocked() {
        return accountNonLocked;
    }

    public void setAccountNonLocked(boolean accountNonLocked) {
        this.accountNonLocked = accountNonLocked;
    }

    public boolean isCredentialsNonExpired() {
        return credentialsNonExpired;
    }

    public void setCredentialsNonExpired(boolean credentialsNonExpired) {
        this.credentialsNonExpired = credentialsNonExpired;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public List<String> getAuthorities() {
        return authorities;
    }

    public void setAuthorities(List<String> authorities) {
        this.authorities = authorities;
    }

    @Override
    public User convertToEntity(ModelMapper modelMapper) {
        return modelMapper.map(this, User.class);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        UserDto userDto = (UserDto) o;
        return accountNonExpired == userDto.accountNonExpired &&
                accountNonLocked == userDto.accountNonLocked &&
                credentialsNonExpired == userDto.credentialsNonExpired &&
                enabled == userDto.enabled &&
                Objects.equals(username, userDto.username) &&
                Objects.equals(authorities, userDto.authorities);
    }

    @Override
    public int hashCode() {
        return Objects.hash(username, accountNonExpired, accountNonLocked, credentialsNonExpired, enabled, authorities);
    }

    public static void main(String[] args) {
        ModelMapper modelMapper = new ModelMapper();
        //        List<Person> people = employees
        //                .stream()
        //                .map(e -> new Person(e.getName()))
        //                .collect(Collectors.toList())

        Converter<String, String> toUppercase =
                ctx -> {
                    return ctx.getSource() == null ? null : ctx.getSource().toUpperCase();
                };

        //        Converter<List<UserRole>, List<String>> listConverter = new Converter<List<UserRole>, List<String>>() {
        //            public List<String> convert(MappingContext<List<UserRole>, List<String>> context) {
        //                List<String> target = new ArrayList<>();
        //                List<UserRole> userRoles = context.getSource();
        //                for (UserRole userRole : userRoles) {
        //                    target.add(userRole.getRole().getName());
        //                }
        //                return target;
        //            }
        //        };

        Converter<List<String>, List<SimpleGrantedAuthority>> sac1 = new AbstractConverter<List<String>, List<SimpleGrantedAuthority>>() {
            @Override
            protected List<SimpleGrantedAuthority> convert(List<String> strings) {
                return strings == null ? null : strings
                        .stream()
                        .map(SimpleGrantedAuthority::new)
                        .collect(Collectors.toList());
            }
        };

        Converter<List<String>, List<SimpleGrantedAuthority>> sac2 = mappingContext -> mappingContext.getSource() == null ? null : mappingContext.getSource()
                .stream()
                .map(SimpleGrantedAuthority::new)
                .collect(Collectors.toList());

        PropertyMap<UserDto, User> propertiesForConvertToEntity = new PropertyMap<UserDto, User>() {
            @Override
            protected void configure() {
                map().setUsername("fake");
                using(sac2).map(source.getAuthorities()).setAuthorities(null);
            }
        };


        modelMapper.addMappings(new PropertyMap<UserDto, User>() {
            @Override
            protected void configure() {
                using(sac2).map(source.getAuthorities()).setAuthorities(null);
            }
        });

        //        modelMapper.addMappings(propertiesForConvertToEntity);

        //        modelMapper.addMappings(mapper -> mapper.using(ctx -> ((String) ctx.getSource()).toUpperCase())
        //                .map(User::getUsername, UserDto::setUsername));
        //        modelMapper.addConverter(sac2);

        Converter<String, String> toUpperCase =
                ctx -> ctx.getSource() == null ? null : ctx.getSource().toUpperCase();


        modelMapper.createTypeMap(UserDto.class, User.class).addMappings(mapper -> mapper.using(toUpperCase).map(UserDto::getUsername, User::setUsername));


        //                modelMapper.addMappings(mapper -> mapper.using(toUpperCase).map(Person::getName, PersonDTO::setName));
        //        typeMap.addMappings(mapper -> mapper.using(ctx -> ((String) ctx.getSource()).toUpperCase())
        //                .map(Person::getName, PersonDTO::setName));

        List<String> authoritiesDto = new ArrayList<>();
        authoritiesDto.add("ROLE_ADMIN");
        //        authoritiesDto = null;
        UserDto userDto = new UserDto(1L, "pcalouch", authoritiesDto);

        User user = userDto.convertToEntity(modelMapper);
        System.out.println(user.getId());
        System.out.println(user.getUsername());
        System.out.println(user.getAuthorities());

        List<SimpleGrantedAuthority> authoritiesEntity = new ArrayList<>();
        authoritiesEntity.add(new SimpleGrantedAuthority("ROLE_ADMIN"));
        User user2 = new User(1L, "pcalouch", authoritiesEntity);

        UserDto userDto2 = user2.convertToDto(modelMapper);
        System.out.println(userDto2.getId());
        System.out.println(userDto2.getUsername());
        System.out.println(userDto2.getAuthorities());
    }
}
