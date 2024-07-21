      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1972.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1972.CPv  $
      *
      *   Rev 2.2   Jul 24 2017 16:14:56   t13ex27
      *FKI01156: Wartungsauftrag Abgleich-DB2-NFM
      *
      *   Rev 2.1   24 Jan 2007 13:33:02   t13ex39
      *AKI00556: SYSIBM-SYS-TABELLEN
      *
      *   Rev 2.0   01 Nov 2006 13:01:20   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00556!01.11.06! 1.0! NEU                       !WHO       *
      * FKI01156!24.07.17!2.2 ! ANPASSUNG AN NFM VON      ! IFR      *
      *         !        !    ! DB2 V11                   !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1972.
           05 EINGABE1-EINGABE-SATZ-1       PIC X(27990) .
           05  FILELR REDEFINES EINGABE1-EINGABE-SATZ-1.
               15 E-CONNECTIONTYPE          PIC X(08).
               15 E-CONNECTION-ID           PIC X(08).
               15 E-CORRELATION-ID          PIC X(12).
               15 E-AUTH-ID                 PIC X(08).
               15 E-PLAN-NAME               PIC X(08).
240717*        15 E-URIDLRSN                PIC X(06).
               15 E-URIDLRSN                PIC X(10).
240717*        15 E-URIDBEGINPOINT          PIC X(06).
               15 E-URIDBEGINPOINT          PIC X(10).
               15 E-URIDMEMBERID            PIC S9(04) COMP.
               15 E-URIDTIMESTAMP           PIC X(26).
               15 E-URIDDISP                PIC X(01).
240717*        15 E-URIDCOMMITLRSN          PIC X(06).
               15 E-URIDCOMMITLRSN          PIC X(10).
240717*        15 E-URIDCOMMITPOINT         PIC X(06).
               15 E-URIDCOMMITPOINT         PIC X(10).
               15 E-DB-NAME                 PIC X(08).
               15 E-TS-NAME                 PIC X(08).
               15 E-TB-CRAETOR              PIC X(08).
               15 E-TB-NAME                 PIC X(18).
               15 E-TB-SEQNUM               PIC X(03).
               15 E-LLRECTIMESTAMP          PIC X(26).
               15 E-LLPARTNUM               PIC X(02).
               15 E-LLRID                   PIC X(05).
240717*        15 E-LOGRBA                  PIC X(06).
               15 E-LOGRBA                  PIC X(10).
               15 E-CHANCE-TYPE             PIC X(01).
               15 E-UPDATE-TYPE             PIC X(01).

