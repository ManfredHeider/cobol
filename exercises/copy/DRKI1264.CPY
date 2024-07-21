      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1264.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1264.CPv  $
      *
      *   Rev 2.2   May 03 2006 13:29:20   t13ex39
      *AKI00215: Umstellung auf EURO VERSICHERTERTARIF
      *
      *   Rev 2.1   May 03 2006 13:24:30   t13ex39
      *AKI00215: Umstellung auf EURO VERSICHERTERTARIF
      *
      *   Rev 2.0   05 Jul 2001 17:07:02   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1264                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1264                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.07.01/14.46.16
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1264
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1264.
           05  PI-DRKI1264-KEY.


             10 PI-DRKI1264-VETVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1264-VETVERTRAGSID   PIC X(13).


           05  PI-DRKI1264-DATEN.


             10 PI-DRKI1264-VETPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI1264-NI-VETPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI1264-VETSUBTYPE   PIC X(01).
             10 PI-DRKI1264-VETVERSSUMME   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1264-NI-VETVERSSUMME   PIC S9(4) COMP.
             10 PI-DRKI1264-VETVERSSUMMEWKZ   PIC X(01).


      *
       01  PO-DRKI1264.
           05  PO-DRKI1264-RC                 PIC  X(003).
               88 PO-DRKI1264-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1264-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
