      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DROK0052.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DROK0052.CPv  $
      *
      *   Rev 2.2   29 May 2008 11:08:24   t13ex27
      *AOK00004: Ticketprogramm und JCL
      *
      *   Rev 2.1   24 Jan 2007 13:33:58   t13ex39
      *AKI00556: SYSIBM-SYS-TABELLEN
      *
      *   Rev 2.0   Nov 10 2006 16:22:26   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DROK0052                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DROK0052                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  15.11.2006/10:41:25
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
      *    FÜR DIE FUNKTION DROK0052
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DROK0052.
           05  PI-DROK0052-KEY.


             10 PI-DROK0052-AK-KEY   PIC X(26).


           05  PI-DROK0052-DATEN.


             10 PI-DROK0052-AK-OKI-ENDE   PIC X(26).
             10 PI-DROK0052-AK-OKI-SATZ-ANZAHL   PIC S9(9) COMP.
             10 PI-DROK0052-AK-OKI-SQL-ANZAHL   PIC S9(9) COMP.
             10 PI-DROK0052-AK-SQL-STATUS   PIC X(10).
             10 PI-DROK0052-AK-STATUS   PIC X(1).
             10 PI-DROK0052-AK-OKI-BIND-ANZ      PIC S9(9) COMP.
             10 PI-DROK0052-AK-OKI-BIND-OKAN     PIC S9(9) COMP.
             10 PI-DROK0052-AK-OKI-SQL-BIND      PIC S9(9) COMP.

      *
       01  PO-DROK0052.
           05  PO-DROK0052-RC                 PIC  X(003).
               88 PO-DROK0052-RC-OK                        VALUE 'OK '.
               88 PO-DROK0052-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
