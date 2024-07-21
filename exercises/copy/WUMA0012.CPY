      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUMA0012.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/WUMA0012.CPv  $
      *
      *   Rev 2.2   Feb 09 2021 16:12:20   t13ac18
      *AMA00359: temp. Auftrag T13AC18 Copies für PMMA5005
      *
      *   Rev 2.1   Jan 23 2014 16:21:22   t13ac20
      *AMA00013: MAGEN200: JCL-Generierung
      *
      *   Rev 2.0   Jan 23 2014 14:14:04   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00013!23.01.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
       01  EXEC-XCHANGE-IND-MAX    pic S9(4) binary sync value +9.
       01  EXEC-XCHANGE.
           05              pic X(80) value
                               '//*=====================================
      -                        '================================
      -                        ''.
           05              pic X(80) value
                               '//*
      -                        '
      -                        ''.
           05              pic X(80) value
                               '//*  XCHANGE DATUM SETZEN
      -                        '
      -                        ''.
           05              pic X(80) value
                               '//*
      -                        '
      -                        ''.
           05              pic X(80) value
                               '//*=====================================
      -                        '================================
      -                        ''.
           05              pic X(80) value
CCC005*                        '//XCHANGE  EXEC PGM=XGCBATCH,
      -                        '
      -                        ''.
CCC004*    05 EXEC-VAR-01  pic X(80) value
CCC004*                       '//         PARM=''DATE=#VAR######,SUBSYS=
      -                       'CWXG''
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//STEPLIB  DD  DSN=P130001.AACWXG00.LOAD
      -                        ',DISP=SHR
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSPRINT DD SYSOUT=*
      -                        '
      -                        ''.
      *
CCC004*01  EXEC-XCHANGEC-IND-MAX   pic S9(4) binary sync value +9.
CCC004*01  EXEC-XCHANGEC.
CCC004*    05              pic X(80) value
CCC004*                        '//*=====================================
      -                        '================================
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//*
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//*  XCHANGE CLEANUP
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//*
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//*=====================================
      -                        '================================
      -                        ''.
CCC004*    05              pic X(80) value
CCC005*                        '//XCHANGEC EXEC PGM=XGCLNREQ,COND=EVEN,
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                      '//         PARM=''SUBSYS=CWXG''
      -                        '
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//STEPLIB  DD  DSN=P130001.AACWXG00.LOAD
      -                        ',DISP=SHR
      -                        ''.
CCC004*    05              pic X(80) value
CCC004*                        '//SYSPRINT DD SYSOUT=*
      -                        '
      -                        ''.
      *
      **************************************************************

