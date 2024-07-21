      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0605.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0605.CPv  $
      *
      *   Rev 2.1   Jun 21 1999 11:11:36   T13EX50
      * 
      *
      *   Rev 2.0   Apr 15 1999 11:30:24   T13EX50
      *Produktionsübergabe - CKV
      *
      *   Rev 1.1   Mar 25 1999 10:37:40   T13EX50
      *Erste Version 
      *
      *   Rev 1.0   25 Aug 1998 09:22:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *01.02.99!NEUERSTELLUNG                          !HERWEG   ! 01  *
      *AKI00022!                                       !         !     *
      *        !                                       !         !     *
      *31.05.99!EINBAU WHR-KZ                          !HERWEG   ! 1.1 *
      *FKI00042!                                       !         !     *
      *        !                                       !         !     *
      * -------!---------------------------------------!---------!-----*
      *08.08.22! Löschen der Felder                    !HENNE    !     *
      *DBO-    ! VS-G-STELLE-NR, VS-G-STELLE-NR-AENDKZ !         !     *
      *10270   ! VS-G-STELLE-TXT und                   !         !     *
      *        ! VS-G-STELLE-TXT-AENDKZ                !         !     *
      *        ! Ersatz durch VS-FILLER                !         !     *
      *        !                                       !         !     *
      ******************************************************************
       01  DRKI0605.
      *
           05  VS-KEY.
               10  VS-GES                     PIC 9(02).
               10  VS-VNR                     PIC 9(15).
      *
           05  VS-VERTRAGSDATEN.
               10  VS-INT-VNR                 PIC 9(07).
               10  VS-INT-VNR-AENDKZ          PIC X(01).
               10  VS-INT-VNR-PRUEF           PIC 9(02).
               10  VS-INT-VNR-PRUEF-AENDKZ    PIC X(01).
               10  VS-VBNR                    PIC X(07).
               10  VS-VBNR-AENDKZ             PIC X(01).
      *
               10  VS-BANK                    PIC X(20).
               10  VS-BANK-AENDKZ             PIC X(01).
               10  VS-BLZ                     PIC 9(11).
               10  VS-BLZ-AENDKZ              PIC X(01).
               10  VS-KTO-NR                  PIC 9(18).
               10  VS-KTO-NR-AENDKZ           PIC X(01).
               10  VS-ZAHLART                 PIC S9(2).
               10  VS-ZAHLART-AENDKZ          PIC X(01).
               10  VS-ZAHLWEISE               PIC S9(2).
               10  VS-ZAHLWEISE-AENDKZ        PIC X(01).
      *
               10  VS-VZUST                   PIC 9(02).
               10  VS-VZUST-AENDKZ            PIC X(01).
               10  VS-VZUST-DAT               PIC X(10).
               10  VS-VZUST-DAT-AENDKZ        PIC X(01).
               10  VS-VAEND-DAT               PIC X(10).
               10  VS-VAEND-DAT-AENDKZ        PIC X(01).
               10  VS-BEITRAG-GES             PIC S9(11)V99.
               10  VS-BEITRAG-GES-AENDKZ      PIC X(01).
               10  VS-BEITRAG-GES-WHR         PIC X(01).
               10  VS-BEITRAG-GES-WHR-AENDKZ  PIC X(01).
      *
               10  VS-NACHLASS-KZ             PIC X(01).
               10  VS-NACHLASS-KZ-AENDKZ      PIC X(01).
               10  VS-GEBUENDELT-KZ           PIC X(01).
               10  VS-GEBUENDELT-KZ-AENDKZ    PIC X(01).
               10  VS-STORNO-DAT              PIC X(10).
               10  VS-STORNO-DAT-AENDKZ       PIC X(01).
               10  VS-VBEGINN                 PIC X(10).
               10  VS-VBEGINN-AENDKZ          PIC X(01).
               10  VS-VABLAUF                 PIC X(10).
               10  VS-VABLAUF-AENDKZ          PIC X(01).
               10  VS-FREISTELL-KZ            PIC X(01).
               10  VS-FREISTELL-KZ-AENDKZ     PIC X(01).
               
           05  VS-FILLER                      PIC X(25).   
      *
           05  VS-AKTUAL-KZ                   PIC X(01).
      *
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *****************************************************************
      *        E N D E  COPY-BUCH   DRKI0605  VERTRAG                 *
      *****************************************************************

