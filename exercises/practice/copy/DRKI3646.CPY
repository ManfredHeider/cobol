      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.03.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3646.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3646.CPv  $
      *
      *   Rev 2.2   Jun 05 2019 11:14:24   t13ex78
      *AKI01669: OZP G67_VERTRAG
      *
      *   Rev 2.1   Apr 15 2019 11:26:04   t13ex78
      *AKI01669: OZP G67_VERTRAG
      *
      *   Rev 2.0   Mar 22 2019 15:42:57 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3646                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3646                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         22.03.2019 / 15:42:57 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI3646
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3646.
           05  PI-DRKI3646-KEY.
 
ANF   *        %M_PI_KEY%
             10 PI-DRKI3646-G67VERTRAGSNR   PIC  X(0015).
END   *
 
           05  PI-DRKI3646-NI.
 
ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *
 
           05  PI-DRKI3646-DATEN.
 
ANF   *        %M_PI_DATEN%
             10 PI-DRKI3646-G67ABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI3646-G67DLBETRAG   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3646-G67FAELLIGKEIT   PIC  X(0008).
             10 PI-DRKI3646-G67KREDITART   PIC  X(0006).
             10 PI-DRKI3646-G67KREDITAUSZ   PIC  X(0008).
             10 PI-DRKI3646-G67KREDITZUS   PIC  X(0008).
             10 PI-DRKI3646-G67MANDANT   PIC S9(9) COMP.
             10 PI-DRKI3646-G67RATE   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3646-G67RESTLAUFZEIT   PIC  X(0003).
             10 PI-DRKI3646-G67SALDO   PIC S9(12)V9(2) COMP-3.
             10 PI-DRKI3646-G67SICHERHEIT   PIC  X(0001).
             10 PI-DRKI3646-G67STATUS   PIC  X(0001).
             10 PI-DRKI3646-G67VRTRBEGINN   PIC  X(0008).
             10 PI-DRKI3646-G67VRTRENDE   PIC  X(0008).
             10 PI-DRKI3646-G67ZINSSATZ   PIC S9(2)V9(4) COMP-3.
END   *
 
      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3646-NI1.
           05 PI-DRKI3646-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI3646-NI7.
           05 PI-DRKI3646-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.
 
      *
       01  PO-DRKI3646.
           05  PO-DRKI3646-RC                     PIC  X(003).
               88 PO-DRKI3646-RC-OK                  VALUE 'OK '.
               88 PO-DRKI3646-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************


