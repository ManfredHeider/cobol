      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2380.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2380.CPv  $
      *
      *   Rev 2.1   Mar 11 2010 10:00:50   t13ex14
      *AKI00935: Std.-OZP für Tabelle LEBENPERSONEIGEN 
      *
      *   Rev 2.0   Feb 25 2010 10:45:36   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2380                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2380                    *
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
      *    FÜR DIE FUNKTION DRKI2380
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2380.
           05  PI-DRKI2380-KEY.


             10 PI-DRKI2380-LPEPERSROLLEKZ   PIC X(1).
             10 PI-DRKI2380-LPEVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2380-DATEN.


             10 PI-DRKI2380-LPEADRESSEID   PIC X(13).
             10 PI-DRKI2380-LPEANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2380-LPEGEBDATUM   PIC X(10).
             10 PI-DRKI2380-LPENACHNAME   PIC X(30).
             10 PI-DRKI2380-LPEVORNAME   PIC X(30).
             
      *
       01  PO-DRKI2380.
           05  PO-DRKI2380-RC                 PIC  X(003).
               88 PO-DRKI2380-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2380-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
