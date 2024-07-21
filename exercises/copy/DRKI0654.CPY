      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0654.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0654.CPv  $
      *
      *   Rev 2.0   Apr 15 1999 11:34:50   T13EX50
      *Produktionsübergabe - CKV
      *
      *   Rev 1.1   Mar 25 1999 10:36:16   T13EX50
      *Erste Version 
      *
      *   Rev 1.0   25 Aug 1998 09:25:02   P13SP99
      *Initial revision.
      *****************************************************************
      *    COPY-MEMBER: DRKI0654                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0654                    *
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
      *    FÜR DIE FUNKTION DRKI0654
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0654.
           05  PI-DRKI0654-KEY.


             10 PI-DRKI0654-KRTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0654-KRTVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI0654.
           05  PO-DRKI0654-RC                 PIC  X(003).
               88 PO-DRKI0654-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0654-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI0654-DATEN.


             10 PO-DRKI0654-KRTBEITRAGVERZ   PIC S9(2)V9(1) COMP-3.
             10 PO-DRKI0654-KRTGESCHAEFTSST   PIC X(20).
             10 PO-DRKI0654-KRTGESCHAEFTSSTID   PIC S9(4) COMP.
             10 PO-DRKI0654-KRTPERSONENNUMMER   PIC S9(4) COMP.


      *
      ******* ENDE COPY-MEMBER **************************************
