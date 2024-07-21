      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1289.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1289.CPv  $
      *
      *   Rev 2.3   02 Feb 2009 10:18:28   t13ex27
      *AKI00768: CR 22257 - Bearbeitungskennzeichen Rechenwerte-DB-
      *Änderung
      *
      *   Rev 2.2   May 03 2006 13:29:08   t13ex39
      *AKI00215: Umstellung auf EURO VERSICHERTERTARIF
      *
      *   Rev 2.1   May 03 2006 13:24:22   t13ex39
      *AKI00215: Umstellung auf EURO VERSICHERTERTARIF
      *
      *   Rev 2.0   05 Jul 2001 17:07:26   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1289                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1289                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  21.01.2009/12:23:21
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
      *    FÜR DIE FUNKTION DRKI1289
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1289.
           05  PI-DRKI1289-KEY.


             10 PI-DRKI1289-VETVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1289-VETVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI1289.
           05  PO-DRKI1289-RC                 PIC  X(003).
               88 PO-DRKI1289-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1289-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI1289-DATEN.


             10 PO-DRKI1289-VETPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI1289-NI-VETPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI1289-VETSUBTYPE   PIC X(01).
             10 PO-DRKI1289-VETVERSSUMME   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1289-NI-VETVERSSUMME   PIC S9(4) COMP.
             10 PO-DRKI1289-VETVERSSUMMEBKZ   PIC X(1).
             10 PO-DRKI1289-VETVERSSUMMEWKZ   PIC X(01).


      *
      ******* ENDE COPY-MEMBER **************************************
