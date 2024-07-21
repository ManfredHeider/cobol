      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0836.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drki0836.cpv  $
      *
      *   Rev 2.1   Apr 29 1999 10:37:04   T13EX19
      *fki00029
      *
      *   Rev 2.0   Feb 27 1999 12:58:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:51:00   P13SP20
      *Initial revision.
      *****************************************************************
      *    COPY-MEMBER: DRKI0836                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0836                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  23.04.99/11.51.21
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI00029!22.04.99!1   !                           !VZG       *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0836
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0836.
           05  PI-DRKI0836-KEY.


             10 PI-DRKI0836-BENMITAROLLENID   PIC X(13).
             10 PI-DRKI0836-BENMITAROLLENKZ   PIC X(01).


           05  PI-DRKI0836-DATEN.


             10 PI-DRKI0836-BENKENNWORT   PIC X(15).


      *
       01  PO-DRKI0836.
           05  PO-DRKI0836-RC                 PIC  X(003).
               88 PO-DRKI0836-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0836-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
