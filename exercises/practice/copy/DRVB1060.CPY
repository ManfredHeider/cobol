      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.06.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1060.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB1060.CPv  $
      *
      *   Rev 2.2   Feb 20 2013 11:58:26   t13ex39
      *FKI01406: Return-Code in UPKI0209
      *
      *   Rev 2.1   Jun 25 2012 10:18:26   t13ae30
      *AVB01241: Neue Funktion ZWVB9996
      *
      *   Rev 2.0   Jun 21 2012 18:52:24   t13ae30
      *Archiverstellung
      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AVB01241!21.06.12!1.0 ! NEU                       ! WILLIG   *
      * --------+--------+----+---------------------------+----------*
      * FKI01406!01.02.13!2.0 ! PO-DRVB1060-STUFE jetzt   !          *
      *         !        !    !            PIC S9(04)COMP !          *
      * --------+--------+----+---------------------------+----------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
       01  PI-DRVB1060.
           05  PI-DRVB1060-KEY.
             10 PI-DRVB1060-VBNR            PIC S9(9) COMP.
      *
       01  PO-DRVB1060.
           05  PO-DRVB1060-VBNR               PIC S9(9) COMP.
           05  PO-DRVB1060-KZ                 PIC  X(001).
           05  PO-DRVB1060-AUSDAT             PIC  X(010).
010213*    05  PO-DRVB1060-STUFE              PIC  X(003) .
010213     05  PO-DRVB1060-STUFE              PIC S9(004) COMP.
           05  PO-DRVB1060-RC                 PIC  X(003).
               88 PO-DRVB1060-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1060-RC-NOT-FOUND                 VALUE 'NFD'.
      *---------------------------------------------------------------*
