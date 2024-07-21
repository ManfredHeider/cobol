      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1822.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1822.CPv  $
      *
      *   Rev 2.3   16 Dec 2008 11:35:48   t13ex27
      *AKI00767: CR 14112 - Historiesierung Riesterdaten-DB-Änderung
      *
      *   Rev 2.2   16 Dec 2008 11:17:24   t13ex27
      *AKI00767: CR 14112 - Historiesierung Riesterdaten-DB-Änderung
      *
      *   Rev 2.1   Sep 20 2005 08:44:38   t13ex49
      *AKI00483: Riester Förderung - OPC-Änderungen - BTGVERF - OZP
      *
      *   Rev 2.0   Jun 23 2005 11:06:20   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1822                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1822                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  16.12.2008/10:59:56
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
      *    FÜR DIE FUNKTION DRKI1822
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1822.
           05  PI-DRKI1822-KEY.


             10 PI-DRKI1822-RZLVERTRAGSID   PIC X(13).
             10 PI-DRKI1822-RZLZULAGENJAHR   PIC S9(4) COMP.
             
      *
       01  PO-DRKI1822.
           05  PO-DRKI1822-RC                 PIC  X(003).
               88 PO-DRKI1822-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1822-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
