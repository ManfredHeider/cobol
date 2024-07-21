      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.01.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2372.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2372.CPv  $
      *
      *   Rev 2.1   Feb 09 2010 12:29:50   t13ex14
      *AKI00913: Löschung in Aktualisierung unterdrücken u. sep. Lö
      *schprogramm
      *
      *   Rev 2.0   Jan 18 2010 12:33:38   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2372                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2372                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00913!18.01.10!    ! NEU                       ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2372
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  DELETE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2372.
           05  PI-DRKI2372-KEY.


             10 PI-DRKI2372-UMSGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2372-UMSLAUFENDENR   PIC S9(4) COMP.
             10 PI-DRKI2372-UMSVERTRAGSID   PIC X(13).

      *
       01  PO-DRKI2372.
           05  PO-DRKI2372-RC                 PIC  X(003).
               88 PO-DRKI2372-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2372-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRKI2372-DATEN.
             10  PO-DRKI2372-ANZDEL           PIC S9(9) COMP.






      ******* ENDE COPY-MEMBER **************************************
