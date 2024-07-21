      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   UU266S.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/UU266S.CPv  $
      *
      *   Rev 2.2   Apr 10 2006 16:32:16   t13ae28
      *ASL00216: Druck VSW-Brief
      *
      *   Rev 2.1   Sep 23 1999 11:16:10   T13AE42
      *ASL00013 - Anpassung VSW99
      *
      *   Rev 2.0   Feb 27 1999 15:15:20   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 09:58:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****  VERSION   2    11.11.92 10:03:16
      ******************************************************
      *                  COPY-MEMBER                       *
      ******************************************************
      *                                                    *
      *  BEZEICHNUNG       : BRIEFDATEN VSW                *
      *  DATEINUMMER       : UU0266                        *
      *  COPYMEMBER        : UU266S                        *
      *  SATZLAENGE        : 150 BYTES                     *
      *  DAVON FILLER      :  43 BYTES                     *
      *  ERSTELLDATUM      : 28.11.1990                    *
      *                                                    *
      *  ÄNDERUNGEN:                                       *
      *  UU266S-AUSL EINGEFÜGT, FILLER VERKÜRZT  VERS.: 02 *
      *                      10.11.92/GSELLHOFER           *
      ******************************************************
090999*LOGBUCH EINGEFUEGT
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00013!09.09.99! 03 ! ERWEITERUNG FUER ABBRUCH- !          *
      *         !        !    ! LISTE                     !SCHMIDT,B.*
      * ASL00216!04.04.06! 04 ! ERWEITERUNG FUER NEUE     !          *
      *         !        !    ! TARIFE                    !D.EHRMANN *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************


       01  UU266S-REC.
           05  UU266S-DIR              PIC 9(03).
           05  UU266S-VBNR             PIC 9(07).
           05  FILLER                  REDEFINES UU266S-VBNR.
               10  UU266S-VBNR-1       PIC 9(03).
               10  UU266S-VBNR-2       PIC 9(03).
           05  UU266S-NACHNAME         PIC X(20).
           05  UU266S-VORNAME          PIC X(20).
           05  UU266S-STRASSE          PIC X(20).
           05  UU266S-PLZ              PIC X(06).
           05  UU266S-ORT              PIC X(16).
           05  UU266S-ANREDE           PIC 9(01).
           05  UU266S-GV-VORJAHR       PIC X(01).
           05  UU266S-GV-LFDJAHR       PIC X(01).
           05  UU266S-GV-ZV-STUETZ     PIC X(01).
           05  UU266S-ZV-II            PIC X(01).
           05  UU266S-AV-VORJAHR       PIC X(01).
           05  UU266S-AV-ZV-LFDJAHR    PIC X(01).
           05  UU266S-AV-D7-LFDJAHR    PIC X(01).
           05  UU266S-AV-HG-LFDJAHR    PIC X(01).
           05  UU266S-IG-VSW           PIC X(01).
           05  UU266S-FAP-STUFE        PIC 9(01).
           05  UU266S-D7-STUFE         PIC 9(01).
           05  UU266S-D7-HOLOG         PIC X(01).
           05  UU266S-UEBERWSG-AV      PIC 9(06).
           05  UU266S-UEBERWSG-D7      PIC 9(06).
           05  UU266S-UEBERWSG-HG      PIC 9(06).
           05  UU266S-UEBERWSG-AG      PIC 9(06).
           05  UU266S-G-VSU            PIC 9(07)V9(02).
           05  UU266S-G-VSNR           PIC 9(10).
           05  UU266S-G-ABLAUFLSTG     PIC 9(07)V9(02).
           05  UU266S-AUSL             PIC X(03).
090999     05  UU266S-UEBERWSG-GV-VSW  PIC 9(06).
090999     05  UU266S-UEBERWSG-GV-ZV   PIC 9(06).
090999     05  UU266S-UEBERWSG-AE      PIC 9(06).
090999     05  UU266S-ED-ANZ           PIC 9(02).
090999     05  UU266S-UEBERWSG-GESAMT  PIC 9(06).
090999*    05  UU266S-FILLER           PIC X(40).
040406     05  UU266S-RV-RENTE         PIC 9(07)V9(02).
040406     05  UU266S-RV-ABF           PIC 9(07)V9(02).
040406     05  UU266S-RV-BURENTE       PIC 9(07)V9(02).
040406     05  UU266S-RV-VERS-SUM      PIC 9(07)V9(02).
040406     05  UU266S-FILLER           PIC X(11).
