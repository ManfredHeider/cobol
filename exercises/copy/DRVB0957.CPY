      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0957.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0957.CPv  $
      *
      *   Rev 2.2   Jan 18 2010 15:52:50   t13ex39
      *AKI00884: VB-Nr. prüfung über VIEV  VIVB9996
      *
      *   Rev 2.1   Dec 09 2009 11:11:18   t13ex39
      *AKI00884: VB-Nr. prüfung über VIEV  VIVB9996
      *
      *   Rev 2.0   Oct 01 2009 08:56:48   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AKI00844!18.01.10!1.0 ! NEU                       ! WHO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
       01  PI-DRVB0957.
           05  PI-DRVB0957-KEY.


             10 PI-DRVB0957-VBNR            PIC S9(9) COMP.

      *
       01  PO-DRVB0957.
           05  PO-DRVB0957-KZ                 PIC  X(001).
           05  PO-DRVB0957-RC                 PIC  X(003).
               88 PO-DRVB0957-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0957-RC-NOT-FOUND                 VALUE 'NFD'.





