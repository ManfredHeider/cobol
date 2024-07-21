      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2517.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2517.CPv  $
      *
      *   Rev 2.2   Mar 12 2019 07:32:14   t13ac17
      *AKI01664: Garantierte Leistungsdynamik im Falle von BU bzw. 
      *GF - Anpassung Host-Komponenten Standard-OZP
      *
      *   Rev 2.1   Sep 21 2011 10:39:24   t13ex27
      *FKI00911: Verträge löschen-LV
      *
      *   Rev 2.0   Sep 15 2011 18:05:10 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2517                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2517                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         11.02.2019 / 11:06:33 / T13AC17
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
      *    FÜR DIE FUNKTION DRKI2517
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2517.
           05  PI-DRKI2517-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2517-LTTVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI2517.
           05  PO-DRKI2517-RC                     PIC  X(003).
               88 PO-DRKI2517-RC-OK               VALUE 'OK '.
               88 PO-DRKI2517-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2517-DATEN.
             10   PO-DRKI2517-COUNTER           PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

