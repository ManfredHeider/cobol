      ******************************************************************        
      *                                                                *        
      *                                                                *        
      *    COPY-MEMBER ZUR FORMALEN AUFBEREITUNG LAUFENDE AKTUALISIER- *        
      *    UNG AML                                                     *        
      *                - Teilschicht AML -                             *        
      *                                                                *        
      *    BETROFFENE                                                  *        
      *    TABELLEN    LEBENTEILTYP                                    *        
      *                                                                *        
      *    SATZLÄNGE   50                                              *        
      *                                                                *        
      *----------------------------------------------------------------*        
      *    LOGBUCH                                                     *        
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*        
      *AUFTRAG !                                       !         !     *        
      *--------!---------------------------------------!---------!-----*        
      *16.08.98!NEUERSTELLUNG                          !S.JOST   ! 01  *        
      *AKI00001!                                       !         !     *        
      *        !                                       !         !     *        
      *        !                                       !         !     *        
      ******************************************************************        
      *                                                                         
       01  AML-SCHICHT-TEILTYP-SATZ.                                            
           05  T-S-KEY.                                                         
               10  T-S-GES                     PIC 9(02).                       
               10  T-S-VNR                     PIC 9(15).                       
               10  T-S-TEILTYP                 PIC 9(02).                       
      *                                                                         
           05  T-S-TEILTYPDATEN.                                                
               10  T-S-VBEGINN                 PIC 9(08).                       
               10  T-S-AEND-VBEGINN-KZ         PIC X(01).                       
               10  T-S-VABLAUF                 PIC 9(08).                       
               10  T-S-AEND-VABLAUF-KZ         PIC X(01).                       
               10  T-S-BZAHL-DAUER             PIC 9(08).                       
               10  T-S-AEND-BZAHL-DAUER-KZ     PIC X(01).                       
      *                                                                         
           05  T-S-AKTUAL-KZ                   PIC X(01).                       
           05  T-S-RESERVE                     PIC X(03).                       
