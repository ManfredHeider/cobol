      *****************************************************************         
      *            COPY-MEMBER: DRCV0034                              *         
      *                                                               *         
      *                                                               *         
      *            Inhalt    :  Parameter für Rechnung viaNova        *         
      *            Datei     :  CV018500                              *         
      *            Member    :  DRCV0034                              *         
      *            Organisat.:  VSAM                                  *         
      *            Satzlaenge:  253 Byte                              *         
      *            Key       :  VBNR,PosNr (13 Byte)                  *         
      *                                                               *         
      *                                                               *         
      *                                                               *         
      *                                                               *         
      *                                                               *         
      *****************************************************************         
      * LOGBUCH:                                                      *         
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *         
      * --------------------------------------------------------------*         
      * VLA-4848   !30.12.22! neu                          !J.Hering  *         
      * --------------------------------------------------------------*         
      *            !        !                              !          *         
      *            !        !                              !          *         
      *                                                               *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
       01  DRCV0034-SATZ.                                                       
           05 DRCV0034-KEY.                                                     
              10 DRCV0034-VBNR                     PIC 9(007).                  
              10 DRCV0034-POSNR                    PIC 9(006).                  
           05 DRCV0034-STICHTAG                    PIC X(10).                   
           05 DRCV0034-POSTXT.                                                  
              10 DRCV0034-EINEIT                   PIC X(12).                   
              10 FILLER                            PIC X(01).                   
              10 DRCV0034-ZEITRAUM                 PIC X(25).                   
              10 FILLER                            PIC X(01).                   
              10 DRCV0034-MWST-SATZ                PIC X(05).                   
              10 FILLER                            PIC X(01).                   
              10 DRCV0034-STUECK-ZAHL              PIC X(04).                   
              10 FILLER                            PIC X(01).                   
           05 DRCV0034-PRODBEZ                     PIC X(60).                   
           05 DRCV0034-LIZNEHMER                   PIC X(60).                   
           05 DRCV0034-BELEGNR                     PIC 9(006).                  
           05 DRCV0034-PRPEH                       PIC 9(006)V99.               
           05 DRCV0034-MWSTB                       PIC 9(006)V99.               
           05 DRCV0034-GESPR                       PIC 9(006)V99.               
      *---------------------- ENDE DRCV0034 --------------------------*         
