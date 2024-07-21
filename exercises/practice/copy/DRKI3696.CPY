      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.06.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3696.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3696.CPv  $
      *
      *   Rev 2.1   Aug 27 2019 14:52:28   t13ex78
      *AKI01692: Standard-OZPs
      *
      *   Rev 2.0   Jun 24 2019 13:22:30 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3696                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3696                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         20.08.2019 / 08:14:38 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3696
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  NEXT VALUE<<<
      *
      ****************************************************************
      *
       01  PI-DRKI3696.
           05  PI-DRKI3696-KEY.
 
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3696-DIRVERTRAGSID   PIC  X(0013).
END   *
 
 
           05  PI-DRKI3696-DATEN.
 
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3696-DIRBUCHDAT   PIC  X(010).
             10 PI-DRKI3696-DIREMAIL.
                49 PI-DRKI3696-DIREMAIL-LEN PIC S9(4) COMP.
                49 PI-DRKI3696-DIREMAIL-TEXT PIC X(40).
             10 PI-DRKI3696-DIRGRAMM   PIC S9(10)V9(2) COMP-3.
             10 PI-DRKI3696-DIRMETALL   PIC  X(0001).
             10 PI-DRKI3696-DIRPRODBEZ.
                49 PI-DRKI3696-DIRPRODBEZ-LEN PIC S9(4) COMP.
                49 PI-DRKI3696-DIRPRODBEZ-TEXT PIC X(30).
             10 PI-DRKI3696-DIRPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI3696-DIRRABATT   PIC  X(0001).
             10 PI-DRKI3696-DIRSTUECKZAHL   PIC S9(4) COMP.
             10 PI-DRKI3696-DIRUMSATZ   PIC S9(7)V9(2) COMP-3.
END   *
 
      *
       01  PO-DRKI3696.
           05  PO-DRKI3696-RC                     PIC  X(003).
               88 PO-DRKI3696-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3696-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
           05  PO-DRKI3696-DATEN.
 
 
ANF   *        %M_PO_DATEN%
             10 PO-DRKI3696-DIRLFDNR   PIC S9(4) COMP.
END   *
 
 
      *
      ******* ENDE COPY-MEMBER **************************************

