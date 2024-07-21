      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3694.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3694.CPv  $
      *
      *   Rev 2.1   Aug 27 2019 14:51:20   t13ex78
      *AKI01692: Standard-OZPs
      *
      *   Rev 2.0   Jun 24 2019 13:22:26 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3694                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3694                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.08.2019 / 08:14:33 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3694
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3694.
           05  PI-DRKI3694-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3694-DIRLFDNR   PIC S9(4) COMP.
             10 PI-DRKI3694-DIRVERTRAGSID   PIC  X(0013).
END   *
 
           05  PI-DRKI3694-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3694-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3694-DIRBUCHDAT   PIC  X(010).
             10 PI-DRKI3694-DIREMAIL.
                49 PI-DRKI3694-DIREMAIL-LEN PIC S9(4) COMP.
                49 PI-DRKI3694-DIREMAIL-TEXT PIC X(40).
             10 PI-DRKI3694-DIRGRAMM   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI3694-DIRMETALL   PIC  X(0001).
             10 PI-DRKI3694-DIRPRODBEZ.
                49 PI-DRKI3694-DIRPRODBEZ-LEN PIC S9(4) COMP.
                49 PI-DRKI3694-DIRPRODBEZ-TEXT PIC X(30).
             10 PI-DRKI3694-DIRPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI3694-DIRRABATT   PIC  X(0001).
             10 PI-DRKI3694-DIRSTUECKZAHL   PIC S9(4) COMP.
             10 PI-DRKI3694-DIRUMSATZ   PIC S9(7)V9(2) COMP-3.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3694-NI1.
           05 PI-DRKI3694-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3694-NI7.
           05 PI-DRKI3694-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3694.
           05  PO-DRKI3694-RC                     PIC  X(003).
               88 PO-DRKI3694-RC-OK               VALUE 'OK '.
               88 PO-DRKI3694-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

