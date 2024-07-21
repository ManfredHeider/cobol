      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0299.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drat0299.cpv  $
      *
      *   Rev 2.1   Sep 02 1999 12:09:44   T13AE30
      * 
      *
      *   Rev 2.0   Mar 16 1999 09:57:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRAT0299                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPAT0026                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: THORSTEN WILLIG                                  *
      *    DATUM:    30.08.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!30.08.99! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0298                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRAT0299.
          05 PI-DRAT0299-KTONR-ALPHANUM         PIC X(020).
          05 PI-DRAT0299-BLZ-ALPHANUM           PIC X(011).
      *
       01 PO-DRAT0299.

          05 PO-DRAT0299-RC                  PIC  X(003).
             88 PO-DRAT0299-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0299-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0299-RC-NOT-OK                    VALUE 'NOK'.

          05 PO-DRAT0299-DATEN.
             10 PO-DRAT0299-KTONR-NUM        PIC  9(018).
             10 PO-DRAT0299-BLZ-NUM          PIC  9(011).
             10 PO-DRAT0299-EX-ALPHA         PIC  X(002).

      *
      ***** ENDE COPY-MEMBER DRAT0299 *****************************


