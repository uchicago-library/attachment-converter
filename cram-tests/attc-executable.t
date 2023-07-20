Test for attc executable
  $ attc --help
  ATTC(1)                           Attc Manual                          ATTC(1)
  
  NNAAMMEE
         attc - Converts email attachments.
  
  SSYYNNOOPPSSIISS
         aattttcc [_O_P_T_I_O_N]… [_A_R_G]
  
  OOPPTTIIOONNSS
         ----ccoonnffiigg=_P_A_T_H
             Sets the absolute path _P_A_T_H to be checked for a configuration file.
  
         --rr, ----rreeppoorrtt
             Provides a list of all attachment types in a given mailbox.
  
         ----rreeppoorrtt--ppaarraammss
             Prints a list of all MIME types in the input along with all header
             and field parameters that go with it.
  
         ----ssiinnggllee--eemmaaiill
             Converts email attachments assuming the input is a single plain
             text email.
  
  CCOOMMMMOONN OOPPTTIIOONNSS
         ----hheellpp[=_F_M_T] (default=aauuttoo)
             Show this help in format _F_M_T. The value _F_M_T must be one of aauuttoo,
             ppaaggeerr, ggrrooffff or ppllaaiinn. With aauuttoo, the format is ppaaggeerr or ppllaaiinn
             whenever the TTEERRMM env var is dduummbb or undefined.
  
  EEXXIITT SSTTAATTUUSS
         aattttcc exits with:
  
         0   on success.
  
         123 on indiscriminate errors reported on standard error.
  
         124 on command line parsing errors.
  
         125 on unexpected internal errors (bugs).
  
  Attc                                                                   ATTC(1)
