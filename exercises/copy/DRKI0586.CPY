      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0586.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRKI0586.CPv  $
      *
      *   Rev 2.1   Apr 12 2000 10:41:04   T13EX39
      *AKI00065 Support-Tabellen
      *
      *   Rev 1.0   07 Aug 1998 14:49:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0586                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0586                    *
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
      *    FÜR DIE FUNKTION DRKI0586
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0586.
           05  PI-DRKI0586-KEY.


             10 PI-DRKI0586-WABHPFLGRZUSATZKZ   PIC X(01).
             10 PI-DRKI0586-WABVERSTARIFLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0586-WABVERTRAGSID   PIC X(13).
             10 PI-DRKI0586-WABWAGNISID   PIC S9(9) COMP.
             10 PI-DRKI0586-WABWAGNISLFDNR   PIC S9(4) COMP.


           05  PI-DRKI0586-DATEN.


             10 PI-DRKI0586-WABWAGNISKZ   PIC X(01).
             10 PI-DRKI0586-WABWAGNISTEXT.
                15 PI-DRKI0586-WABWAGNISTEXT-LEN   PIC S9(4) COMP.
                15 PI-DRKI0586-WABWAGNISTEXT-TEXT   PIC X(25).



      *
       01  PO-DRKI0586.
           05  PO-DRKI0586-RC                 PIC  X(003).
               88 PO-DRKI0586-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0586-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
