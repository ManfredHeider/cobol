      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2389.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2389.CPv  $
      *
      *   Rev 2.3   Sep 12 2016 10:59:20   t13ex78
      *AKI01463: OZP's, Copies ect.
      *
      *   Rev 2.2   Sep 06 2012 14:28:14   t13ae49
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.1   Sep 06 2012 10:58:16   t13ex14
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.0   Mar 22 2010 07:41:46   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2389                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2389                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.09.2016 / 10:06:45 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2389
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2389.
           05  PI-DRKI2389-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2389-KPEPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2389-KPEVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI2389-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2389-NI-KPEVORVENDDATUM PIC S9(4) COMP.
             15 PI-DRKI2389-NI-KPEVORVGES-KNR  PIC S9(4) COMP.
             15 PI-DRKI2389-NI-KPEVORVGES-KTY  PIC S9(4) COMP.
             15 PI-DRKI2389-NI-KPEVORVKUENDKZ  PIC S9(4) COMP.
END   *
 
           05  PI-DRKI2389-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI2389-KPEADRESSEID   PIC  X(0013).
             10 PI-DRKI2389-KPEANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2389-KPEBEGINNDATUM   PIC  X(010).
             10 PI-DRKI2389-KPEENDEDATUM   PIC  X(010).
             10 PI-DRKI2389-KPEGEBDATUM   PIC  X(010).
             10 PI-DRKI2389-KPEGESCHSTID-KNR   PIC S9(4) COMP.
             10 PI-DRKI2389-KPENACHNAME   PIC  X(0030).
             10 PI-DRKI2389-KPEPERSONENID   PIC  X(0013).
             10 PI-DRKI2389-KPEPERSONENNUMMER   PIC S9(4) COMP.
             10 PI-DRKI2389-KPEPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2389-KPEVORNAME   PIC  X(0030).
             10 PI-DRKI2389-KPEVORVENDDATUM   PIC  X(010).
             10 PI-DRKI2389-KPEVORVGES-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2389-KPEVORVGES-KTY   PIC S9(4) COMP.
             10 PI-DRKI2389-KPEVORVKUENDKZ   PIC  X(0001).
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2389-NI1.
           05 PI-DRKI2389-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 4.
       01  PI-DRKI2389-NI7.
           05 PI-DRKI2389-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 4.
      *
       01  PO-DRKI2389.
           05  PO-DRKI2389-RC                     PIC  X(003).
               88 PO-DRKI2389-RC-OK               VALUE 'OK '.
               88 PO-DRKI2389-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

