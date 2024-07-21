      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0029.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0029.CPv  $
      *
      *   Rev 2.1   Dec 02 2011 16:02:04   t13ae34
      *FAN00986: AML: Abweichender Antragstatus VBI/AVS und Host-DVAG
      *
      *   Rev 2.0   Nov 29 2011 21:24:28   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *  COPY-MEMBER FÜR STVD0703 - ABGERECHNETES GESCHÄFT AVS       *
      *                                                              *
      ****************************************************************
      * LOGBUCH                                                      *
      * AUFTRAG ! DATUM  ! AENDERUNG                 ! NAME          *
      * --------+--------+---------------------------+---------------*
      * FAN00986!29.11.11! NEU                       ! HEIDER        *
      *         !        !                           !               *
      ****************************************************************
       01  STVD0703-SATZ.
           03 STVD0703-TYP             PIC S9(4) COMP.
           03 STVD0703-ID              PIC S9(15)V.
           03 STVD0703-AVERM           PIC S9(9) COMP.
           03 STVD0703-PGEB-GES        PIC S9(4) COMP.
           03 STVD0703-VNR.
              05 STVD0703-VNR-1-4      PIC X(04).
              05 STVD0703-VNR-5-12     PIC X(08).
              05 STVD0703-VNR-13-15    PIC X(03).
           03 STVD0703-PIN             PIC S9(9) COMP.
           03 STVD0703-PROD-PROD-SCHL  PIC S9(4) COMP.
           03 STVD0703-KUNDE-NAME      PIC X(25).
           03 STVD0703-KUNDE-VORNAME   PIC X(20).
           03 STVD0703-BEITRAG         PIC S9(11)V9(02) COMP-3.
           03 STVD0703-SUMME           PIC S9(11)V9(02) COMP-3.
           03 STVD0703-EINHEITEN       PIC S9(08)V9(02) COMP-3.
           03 STVD0703-ABGERECHNET-AM  PIC X(26).
           03 STVD0703-VB-ABRECHNUNG   PIC X(10).
      *---------------------------------------------------------------*
