      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0060.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DROK0060.CPv  $
      *
      *   Rev 2.1   24 Jan 2007 13:34:26   t13ex39
      *AKI00556: SYSIBM-SYS-TABELLEN
      *
      *   Rev 2.0   15 Nov 2006 10:54:12   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0060                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0060                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  15.11.2006/11:09:09
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DROK0060
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0060.
           05  PI-DROK0060-KEY.


             10 PI-DROK0060-AK-KEY   PIC X(26).
             
      *
       01  PO-DROK0060.
           05  PO-DROK0060-RC                 PIC  X(003).
               88 PO-DROK0060-RC-OK                        VALUE 'OK '.
               88 PO-DROK0060-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DROK0060-DATEN.


             10 PO-DROK0060-AK-OKI-ENDE   PIC X(26).
             10 PO-DROK0060-AK-OKI-SATZ-ANZAHL   PIC S9(9) COMP.
             10 PO-DROK0060-AK-OKI-SQL-ANZAHL   PIC S9(9) COMP.
             10 PO-DROK0060-AK-OKI-START   PIC X(26).
             10 PO-DROK0060-AK-SATZ-ANZAHL   PIC S9(9) COMP.
             10 PO-DROK0060-AK-SQL-ANZAHL   PIC S9(9) COMP.
             10 PO-DROK0060-AK-SQL-ENDE   PIC X(26).
             10 PO-DROK0060-AK-SQL-START   PIC X(26).
             10 PO-DROK0060-AK-SQL-STATUS   PIC X(10).
             10 PO-DROK0060-AK-STATUS   PIC X(1).
             
           05 PO-DROK0060-AK-KEY   PIC X(26).

      *
      ******* ENDE COPY-MEMBER **************************************
