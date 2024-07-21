      * T13EX39   Stand: 28/07/98    17:04:38
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0308.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0308.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:54:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 16:18:32   T13EX39
      *FUNKTION INSERT FUER ZTKI0357
      *
      *   Rev 1.0   07 Jul 1998 11:50:54   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0308                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0308                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  27.08.98/12.59.21
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
      *    FÜR DIE FUNKTION DRKI0308
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0308.
           05  PI-DRKI0308-KEY.


             10 PI-DRKI0308-VERVERTRAGSID   PIC X(13).


           05  PI-DRKI0308-DATEN.


             10 PI-DRKI0308-VERAENDERUNGSDATUM   PIC X(26).
             10 PI-DRKI0308-VERGESAMTBEITRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0308-VERNACHLASSKZ   PIC X(01).
             10 PI-DRKI0308-VERVERTRAGSABLAUF   PIC X(10).
             10 PI-DRKI0308-VERVERTRAGSBEGINN   PIC X(10).
             10 PI-DRKI0308-VERVERTRZUSTANDDAT   PIC X(10).
             10 PI-DRKI0308-VERVERTRZUSTANDID   PIC S9(4) COMP.
             10 PI-DRKI0308-VERZAHLUNGSWEISEID   PIC S9(4) COMP.


      *
       01  PO-DRKI0308.
           05  PO-DRKI0308-RC                 PIC  X(003).
               88 PO-DRKI0308-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0308-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
