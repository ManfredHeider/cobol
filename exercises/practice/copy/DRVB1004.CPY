      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1004.CPv  $
      *
      *   Rev 2.1   Mar 10 2011 10:42:32   t13ae30
      *AAS00947: AVERM im Stammdaten DTA nach AC
      *
      *   Rev 2.0   Jan 28 2011 19:02:08   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * AAS00947!31.01.11!1.0 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
       01  PI-DRVB1004.
           05  PI-DRVB1004-KEY.


             10 PI-DRVB1004-VBNR            PIC S9(9) COMP.

      *
       01  PO-DRVB1004.
           05  PO-DRVB1004-KZ                 PIC  X(001).
           05  PO-DRVB1004-RC                 PIC  X(003).
               88 PO-DRVB1004-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1004-RC-NOT-FOUND                 VALUE 'NFD'.





