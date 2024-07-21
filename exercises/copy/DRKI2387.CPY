      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2387.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2387.CPv  $
      *
      *   Rev 2.3   Sep 12 2016 10:59:28   t13ex78
      *AKI01463: OZP's, Copies ect.
      *
      *   Rev 2.2   Sep 06 2012 14:28:00   t13ae49
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.1   Sep 06 2012 10:58:00   t13ex14
      *AKI00943: Std.-OZP für Tabelle KRANKENPERSEIGEN
      *
      *   Rev 2.0   Mar 22 2010 07:41:34   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2387                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2387                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         06.09.2016 / 10:06:41 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2387
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2387.
           05  PI-DRKI2387-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI2387-KPEPERSONLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2387-KPEVERTRAGSID   PIC  X(0013).
END   *
 
      *
       01  PO-DRKI2387.
           05  PO-DRKI2387-RC                     PIC  X(003).
               88 PO-DRKI2387-RC-OK               VALUE 'OK '.
               88 PO-DRKI2387-RC-NOT-FOUND        VALUE 'NFD'.
      *
ANF   *        %M_PO_DATEN_NI%
END   *
 
           05  PO-DRKI2387-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI2387-KPEADRESSEID   PIC  X(0013).
             10 PO-DRKI2387-KPEANREDE-KNR   PIC S9(4) COMP.
             10 PO-DRKI2387-KPEBEGINNDATUM   PIC  X(010).
             10 PO-DRKI2387-KPEENDEDATUM   PIC  X(010).
             10 PO-DRKI2387-KPEGEBDATUM   PIC  X(010).
             10 PO-DRKI2387-KPEGESCHSTID-KNR   PIC S9(4) COMP.
             10 PO-DRKI2387-KPENACHNAME   PIC  X(0030).
             10 PO-DRKI2387-KPEPERSONENID   PIC  X(0013).
             10 PO-DRKI2387-KPEPERSONENNUMMER   PIC S9(4) COMP.
             10 PO-DRKI2387-KPEPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI2387-KPEVORNAME   PIC  X(0030).
             10 PO-DRKI2387-KPEVORVENDDATUM   PIC  X(010).
             10 PO-DRKI2387-KPEVORVGES-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI2387-KPEVORVGES-KTY   PIC S9(4) COMP.
             10 PO-DRKI2387-KPEVORVKUENDKZ   PIC  X(0001).
END   *
 
 
      *
      ******* ENDE COPY-MEMBER ***************************************

