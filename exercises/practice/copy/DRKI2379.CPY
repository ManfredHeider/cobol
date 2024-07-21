      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2379.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2379.CPv  $
      *
      *   Rev 2.1   Mar 11 2010 10:00:46   t13ex14
      *AKI00935: Std.-OZP für Tabelle LEBENPERSONEIGEN 
      *
      *   Rev 2.0   Feb 25 2010 10:45:30   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2379                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2379                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  09.03.2010/14:18:14
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
      *    FÜR DIE FUNKTION DRKI2379
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2379.
           05  PI-DRKI2379-KEY.


             10 PI-DRKI2379-LPEPERSROLLEKZ   PIC X(1).
             10 PI-DRKI2379-LPEVERTRAGSID   PIC X(13).
             
      *
       01  PO-DRKI2379.
           05  PO-DRKI2379-RC                 PIC  X(003).
               88 PO-DRKI2379-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2379-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2379-DATEN.


             10 PO-DRKI2379-LPEADRESSEID   PIC X(13).
             10 PO-DRKI2379-LPEANREDE-KNR   PIC S9(4) COMP.
             10 PO-DRKI2379-LPEGEBDATUM   PIC X(10).
             10 PO-DRKI2379-LPENACHNAME   PIC X(30).
             10 PO-DRKI2379-LPEVORNAME   PIC X(30).
             

      *
      ******* ENDE COPY-MEMBER **************************************
