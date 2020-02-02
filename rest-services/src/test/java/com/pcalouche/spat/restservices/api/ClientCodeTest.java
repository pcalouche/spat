package com.pcalouche.spat.restservices.api;

import com.pcalouche.spat.restservices.AbstractTest;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.MalformedJwtException;
import org.junit.Test;
import org.springframework.security.authentication.*;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

import static org.assertj.core.api.Assertions.assertThat;

public class ClientCodeTest extends AbstractTest {
    @Test
    public void testExceptionWithNoClientCode() {
        ClientCode clientCode = ClientCode.fromException(new RuntimeException("message"));
        assertThat(clientCode).isNull();
    }

    @Test
    public void testBadCredentialsExceptionClientCode() {
        ClientCode clientCode = ClientCode.fromException(new BadCredentialsException("message"));
        assertThat(clientCode).isEqualTo(ClientCode.BAD_CREDENTIALS);
    }

    @Test
    public void testUsernameNotFoundExceptionClientCode() {
        ClientCode clientCode = ClientCode.fromException(new UsernameNotFoundException("message"));
        assertThat(clientCode).isEqualTo(ClientCode.BAD_CREDENTIALS);
    }

    @Test
    public void testAccountExpiredExceptionClientCode() {
        ClientCode clientCode = ClientCode.fromException(new AccountExpiredException("message"));
        assertThat(clientCode).isEqualTo(ClientCode.ACCOUNT_EXPIRED);
    }

    @Test
    public void testCredentialsExpiredExceptionClientCode() {
        ClientCode clientCode = ClientCode.fromException(new CredentialsExpiredException("message"));
        assertThat(clientCode).isEqualTo(ClientCode.ACCOUNT_CREDENTIALS_EXPIRED);
    }

    @Test
    public void testLockedExceptionClientCode() {
        ClientCode clientCode = ClientCode.fromException(new LockedException("message"));
        assertThat(clientCode).isEqualTo(ClientCode.ACCOUNT_LOCKED);
    }

    @Test
    public void testDisabledExceptionClientCode() {
        ClientCode clientCode = ClientCode.fromException(new DisabledException("message"));
        assertThat(clientCode).isEqualTo(ClientCode.ACCOUNT_DISABLED);
    }

    @Test
    public void testExpiredJwtExceptionClientCode() {
        ClientCode clientCode = ClientCode.fromException(new ExpiredJwtException(null, null, "message"));
        assertThat(clientCode).isEqualTo(ClientCode.EXPIRED_TOKEN);
    }

    @Test
    public void testInvalidJwtExceptionClientCode() {
        ClientCode clientCode = ClientCode.fromException(new MalformedJwtException("message"));
        assertThat(clientCode).isEqualTo(ClientCode.INVALID_TOKEN);
    }
}
