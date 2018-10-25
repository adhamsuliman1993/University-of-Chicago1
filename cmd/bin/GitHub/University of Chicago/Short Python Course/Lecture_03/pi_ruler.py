def pi_ruler(r):
    
    """
    the circumference of a circle is 2*pi*r (r radius)     
    a circle with r = 1/2 has a circumference pi    
    this method calculates the circumference of a circle in pi units    
    """
    from math import pi
    pi_len = 2*r
    print('a circle with a radius {} inches has a circumference of {} pi'.format(r,pi_len))