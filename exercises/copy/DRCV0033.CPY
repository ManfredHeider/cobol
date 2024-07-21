      *****************************************************************         
      *            COPY-MEMBER: DRCV0033                              *         
      *                                                               *         
      *                                                               *         
      *            Inhalt    :  Parameter für Rechnung viaNova        *         
      *            Datei     :  CV018500                              *         
      *            Member    :  DRCV0033                              *         
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
      * VLA-4848   !13.12.22! neu                          !J.Hering  *         
      * --------------------------------------------------------------*         
      *            !        !                              !          *         
      *            !        !                              !          *         
      *                                                               *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
       01  DRCV0033-SATZ.                                                       
           05 DRCV0033-KEY.                                                     
              10 DRCV0033-VBNR                     PIC 9(007).                  
              10 DRCV0033-POSNR                    PIC 9(006).                  
           05 DRCV0033-STICHTAG                    PIC X(10).                   
           05 DRCV0033-POSTXT.                                                  
              10 DRCV0033-EINEIT                   PIC X(12).                   
              10 FILLER                            PIC X(01).                   
              10 DRCV0033-ZEITRAUM                 PIC X(25).                   
              10 FILLER                            PIC X(01).                   
              10 DRCV0033-MWST-SATZ                PIC X(05).                   
              10 FILLER                            PIC X(01).                   
              10 DRCV0033-STUECK-ZAHL              PIC X(04).                   
              10 FILLER                            PIC X(01).                   
           05 DRCV0033-PRODBEZ                     PIC X(60).                   
           05 DRCV0033-LIZNEHMER                   PIC X(60).                   
           05 DRCV0033-BELEGNR                     PIC 9(006).                  
           05 DRCV0033-PRPEH                       PIC 9(006)V99.               
           05 DRCV0033-MWSTB                       PIC 9(006)V99.               
           05 DRCV0033-GESPR                       PIC 9(006)V99.               
      *---------------------- ENDE DRCV0033 --------------------------*         
