      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0655.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0655.CPv  $
      *
      *   Rev 2.0   Apr 15 1999 11:36:44   T13EX50
      *Produktionsübergabe - CKV
      *
      *   Rev 1.1   Mar 25 1999 10:36:02   T13EX50
      *Erste Version 
      *
      *   Rev 1.0   25 Aug 1998 09:25:02   P13SP99
      *Initial revision.
      *****************************************************************
      *    COPY-MEMBER: DRKI0655                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0655                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  07.10.98/15.49.11
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
      *    FÜR DIE FUNKTION DRKI0655
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0655.
           05  PI-DRKI0655-KEY.


             10 PI-DRKI0655-KRTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0655-KRTVERTRAGSID   PIC X(13).


           05  PI-DRKI0655-DATEN.


             10 PI-DRKI0655-KRTBEITRAGVERZ   PIC S9(2)V9(1) COMP-3.
             10 PI-DRKI0655-KRTGESCHAEFTSST   PIC X(20).
             10 PI-DRKI0655-KRTGESCHAEFTSSTID   PIC S9(4) COMP.
             10 PI-DRKI0655-KRTPERSONENNUMMER   PIC S9(4) COMP.

      *
       01  PO-DRKI0655.
           05  PO-DRKI0655-RC                 PIC  X(003).
               88 PO-DRKI0655-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0655-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
