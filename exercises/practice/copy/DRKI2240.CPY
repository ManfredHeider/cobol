      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2240.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2240.CPv  $
      *
      *   Rev 2.4   May 27 2014 13:16:04   t13ex39
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF  Z T 
      *K I 0 3 1 3 
      *
      *   Rev 2.3   May 27 2014 13:08:16   t13ex39
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF  Z T 
      *K I 0 3 1 3 
      *
      *   Rev 2.2   Apr 14 2008 16:42:38   t13ex51
      *AKI00708: OnlineKI - Deutsche Bank Depot - Neue Insert und U
      *pdate Funktionen
      *
      *   Rev 2.1   Apr 09 2008 14:30:20   t13ex51
      *AKI00708: OnlineKI - Deutsche Bank Depot - Neue Insert und U
      *pdate Funktionen
      *
      *   Rev 2.0   Apr 09 2008 10:25:26   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2240                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2240                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  14.04.2008/15:41:08
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
      *    FÜR DIE FUNKTION DRKI2240
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2240.
           05  PI-DRKI2240-KEY.


             10 PI-DRKI2240-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2240-GHPVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2240-DATEN.


             10 PI-DRKI2240-GHPBRPERFGESBEG   PIC X(10).
             10 PI-DRKI2240-GHPBRPERFGESEND   PIC X(10).
             10 PI-DRKI2240-GHPBRPERFLFJBEG   PIC X(10).
             10 PI-DRKI2240-GHPBRPERFLFJEND   PIC X(10).
             10 PI-DRKI2240-GHPPERFDAT   PIC X(10).
             10 PI-DRKI2240-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2240-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             

      *
       01  PO-DRKI2240.
           05  PO-DRKI2240-RC                 PIC  X(003).
               88 PO-DRKI2240-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2240-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
