      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0344.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/CTKI0344.CPv  $
      *
      *   Rev 2.5   Mar 30 2020 08:41:48   t13ae31
      *AKI01766: G81 Advocard neue Felder u.a.
      *
      *   Rev 2.4   Jul 21 2009 13:35:24   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.3   Apr 30 2009 08:18:00   t13ex14
      *AKI00805: OZP-Objekte
      *
      *   Rev 2.2   Feb 23 2009 07:30:44   t13ex14
      *AKI00731: Kfz-Versicherungsbedingungen
      *
      *   Rev 2.1   Apr 10 2000 14:24:00   T13EX41
      *AKI00065
      *- Längenänderung Schlüsselfelder
      *
      *   Rev 2.0   Feb 27 1999 12:50:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:34   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:59:04   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00065 !30.03.00!    !-VBBVERSBEDINGUNGID IST    !C. HEINECK*
      *         !        !    ! JETZT INTEGER             !          *
      *         !        !    !                           !          *
      *AKI00731 !15.01.09!    !-VBBVERSBEDINGUNG-TEXT     !WMI       *
      *         !        !    ! auf Länge 50 erweitert    !          *
      *         !        !    !                           !          *
      *AKI00805 !25.02.09!    !-VBBVERSBEDINGKEY ergänzt  !WMI       *
      *         !        !    !                           !          *
      *AKI00817 !19.06.09!    !-VBBBEDINGUNG_KTY/KNR erg. !WMI       *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE VERSBEDINGUNGBEZ TABLE
CCC004*    ( VBBVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      VBBVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      VBBVERSBEDINGUNGID             INTEGER NOT NULL,
CCC004*      VBBVERSBEDINGLFDNR             SMALLINT NOT NULL,
CCC004*      VBBVEBPARTNERGES               SMALLINT NOT NULL,
CCC004*      VBBVERSBEDINGKEY               CHAR(2),
CCC004*      VBBVERSBEDINGUNG               VARCHAR(50) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      VBBBEDINGUNG_KTY               SMALLINT,
CCC004*      VBBBEDINGUNG_KNR               DECIMAL(15, 0),
CCC004*      VBBAVB_KTY                     SMALLINT,
CCC004*      VBBAVB_KNR                     DECIMAL(15, 0)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VERSBEDINGUNGBEZ            *
      ******************************************************************
       01  VERSBEDINGUNGBEZ-GESAMT.
        05 NI-VERSBEDINGUNGBEZ-TAB.
           10 NI-VERSBEDINGUNGBEZ-ELE PIC S9(04) COMP OCCURS 12 TIMES.
        05 NI-VERSBEDINGUNGBEZ REDEFINES NI-VERSBEDINGUNGBEZ-TAB.
           10 NI-VBBVERTRAGSID      PIC S9(4) COMP.
           10 NI-VBBVERSTARLFDNR    PIC S9(4) COMP.
           10 NI-VBBVERSBEDINGUNGID PIC S9(4) COMP.
           10 NI-VBBVERSBEDINGLFDNR PIC S9(4) COMP.
           10 NI-VBBVEBPARTNERGES   PIC S9(4) COMP.
           10 NI-VBBVERSBEDINGKEY   PIC S9(4) COMP.
           10 NI-VBBVERSBEDINGUNG   PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-VBBBEDINGUNG-KTY   PIC S9(4) COMP.
           10 NI-VBBBEDINGUNG-KNR   PIC S9(4) COMP.
           10 NI-VBBAVB-KTY         PIC S9(4) COMP.
           10 NI-VBBAVB-KNR         PIC S9(4) COMP.
        05 DCL-VERSBEDINGUNGBEZ.
           10 VBBVERTRAGSID        PIC X(13).
           10 VBBVERSTARLFDNR      PIC S9(4) USAGE COMP.
           10 VBBVERSBEDINGUNGID   PIC S9(9) USAGE COMP.
           10 VBBVERSBEDINGLFDNR   PIC S9(4) USAGE COMP.
           10 VBBVEBPARTNERGES     PIC S9(4) USAGE COMP.
           10 VBBVERSBEDINGKEY     PIC X(2).
           10 VBBVERSBEDINGUNG.
              49 VBBVERSBEDINGUNG-LEN
                 PIC S9(4) USAGE COMP.
              49 VBBVERSBEDINGUNG-TEXT
                 PIC X(50).
           10 LETZTEAENDERUNG      PIC X(26).
           10 VBBBEDINGUNG-KTY     PIC S9(4) USAGE COMP.
           10 VBBBEDINGUNG-KNR     PIC S9(15)V USAGE COMP-3.
           10 VBBAVB-KTY           PIC S9(4) USAGE COMP.
           10 VBBAVB-KNR           PIC S9(15)V USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************

