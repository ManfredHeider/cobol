      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0584.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0584.CPv  $
      *
      *   Rev 2.1   Apr 12 2000 10:41:02   T13EX39
      *AKI00065 Support-Tabellen
      *
      *   Rev 1.0   07 Aug 1998 14:49:50   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0584                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0584                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  10.04.00/12.20.48
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
      *    FÜR DIE FUNKTION DRKI0584
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0584.
           05  PI-DRKI0584-KEY.


             10 PI-DRKI0584-WABHPFLGRZUSATZKZ   PIC X(01).
             10 PI-DRKI0584-WABVERSTARIFLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0584-WABVERTRAGSID   PIC X(13).
             10 PI-DRKI0584-WABWAGNISID   PIC S9(9) COMP.
             10 PI-DRKI0584-WABWAGNISLFDNR   PIC S9(4) COMP.


           05  PI-DRKI0584-DATEN.


             10 PI-DRKI0584-WABWAGNISKZ   PIC X(01).
             10 PI-DRKI0584-WABWAGNISTEXT.
                15 PI-DRKI0584-WABWAGNISTEXT-LEN   PIC S9(4) COMP.
                15 PI-DRKI0584-WABWAGNISTEXT-TEXT   PIC X(25).


      *
       01  PO-DRKI0584.
           05  PO-DRKI0584-RC                 PIC  X(003).
               88 PO-DRKI0584-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0584-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
