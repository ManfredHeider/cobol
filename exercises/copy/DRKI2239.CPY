      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.04.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2239.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2239.CPv  $
      *
      *   Rev 2.4   May 27 2014 13:16:12   t13ex39
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF  Z T 
      *K I 0 3 1 3 
      *
      *   Rev 2.3   May 27 2014 13:08:22   t13ex39
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF  Z T 
      *K I 0 3 1 3 
      *
      *   Rev 2.2   Apr 14 2008 16:42:32   t13ex51
      *AKI00708: OnlineKI - Deutsche Bank Depot - Neue Insert und U
      *pdate Funktionen
      *
      *   Rev 2.1   Apr 09 2008 14:30:12   t13ex51
      *AKI00708: OnlineKI - Deutsche Bank Depot - Neue Insert und U
      *pdate Funktionen
      *
      *   Rev 2.0   Apr 09 2008 10:25:20   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2239                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2239                    *
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
      *    FÜR DIE FUNKTION DRKI2239
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2239.
           05  PI-DRKI2239-KEY.


             10 PI-DRKI2239-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2239-GHPVERTRAGSID   PIC X(13).
             

           05  PI-DRKI2239-DATEN.


             10 PI-DRKI2239-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2239-GHPANLAGENAME.
                15 PI-DRKI2239-GHPANLAGENAME-LEN   PIC S9(4) COMP.
                15 PI-DRKI2239-GHPANLAGENAME-TEXT   PIC X(15).
                
             10 PI-DRKI2239-GHPANLAGEZIEL   PIC X(1).
             10 PI-DRKI2239-GHPBRPERFGESBEG   PIC X(10).
             10 PI-DRKI2239-GHPBRPERFGESEND   PIC X(10).
             10 PI-DRKI2239-GHPBRPERFLFJBEG   PIC X(10).
             10 PI-DRKI2239-GHPBRPERFLFJEND   PIC X(10).
             10 PI-DRKI2239-GHPDEPOTART   PIC X(1).
             10 PI-DRKI2239-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2239-GHPEIGENVERMITTELT   PIC X(1).
             10 PI-DRKI2239-GHPFAXREVERS   PIC X(1).
             10 PI-DRKI2239-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2239-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PI-DRKI2239-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2239-GHPGUTHABENWKZ   PIC X(1).
             10 PI-DRKI2239-GHPGUTHWERTWKZ   PIC X(1).
             10 PI-DRKI2239-GHPINFOSPERREDATUM   PIC X(10).
             10 PI-DRKI2239-GHPINFOWPERREKZ   PIC X(1).
             10 PI-DRKI2239-GHPOFFNUNGSDATUM   PIC X(10).
             10 PI-DRKI2239-GHPONLINEREVERS   PIC X(1).
             10 PI-DRKI2239-GHPPERFDAT   PIC X(10).
             10 PI-DRKI2239-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2239-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2239-GHPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2239-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI2239-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2239-GHPSCHLAUSZGWKZ   PIC X(1).
             10 PI-DRKI2239-GHPSCHLIESSDATUM   PIC X(10).
             10 PI-DRKI2239-GHPSUBTYPE   PIC X(1).
             10 PI-DRKI2239-GHPTELEFONREVERS   PIC X(1).
             10 PI-DRKI2239-GHPVERFUEGUNGID   PIC X(2).
             10 PI-DRKI2239-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2239-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2239-GHPZWISCHENGEWWKZ   PIC X(1).
             
      *
       01  PO-DRKI2239.
           05  PO-DRKI2239-RC                 PIC  X(003).
               88 PO-DRKI2239-RC-OK                        VALUE 'OK '.
               88 PO-DRKI2239-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
