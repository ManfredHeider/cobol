      * I13PC18   Stand: 20/10/98    10:23:44
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0720.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0720.CPv  $
      *
      *   Rev 2.0   Apr 16 1999 18:04:38   T13EX48
      *AKI00028
      *PI/PO FUNKTION DRKI0720
      *
      *   Rev 1.0   06 Oct 1998 15:32:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *  Update Erledigungskennzeichen
      *****************************************************************
      *    COPY-MEMBER: DRKI0720                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0720                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  14.10.98/10.19.01
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
      *    FÜR DIE FUNKTION DRKI0720
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0720.
           05  PI-DRKI0720-KEY.
             10 PI-DRKI0720-RUECKLAUFKONTRID   PIC X(13).
           05  PI-DRKI0720-DATEN.
             10 PI-DRKI0720-ERLEDIGUNGSKZ   PIC S9(4) COMP.

      *
       01  PO-DRKI0720.
           05  PO-DRKI0720-RC                 PIC  X(003).
               88 PO-DRKI0720-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0720-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
