      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.10.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2365.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2365.CPv  $
      *
      *   Rev 2.3   Jun 28 2011 09:55:34   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g FSA
      *
      *   Rev 2.2   Nov 16 2009 18:29:32   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Einführung für Deutsche Bank
      *
      *   Rev 2.1   Nov 16 2009 17:12:06   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Einführung für Deutsche Bank
      *
      *   Rev 2.0   Oct 19 2009 12:22:42   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2365                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2365                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.06.2011 / 10:55:18 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2365
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2365.
           05  PI-DRKI2365-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2365-FSAPERSONENID   PIC  X(0013).
             10 PI-DRKI2365-FSAVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2365-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2365-FSAANZDEPFREISTBET   PIC S9(4) COMP.
             10 PI-DRKI2365-FSAAUFTKONF1   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2365-NI-FSAAUFTKONF1    PIC S9(4) COMP.
             10 PI-DRKI2365-FSAAUFTKONF2   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2365-FSAAUSLQSTEUER   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSAFREIBETRAGABL   PIC  X(010).
             10 PI-DRKI2365-FSAFREIBETRAGVERBR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSAFREISTAUFTRNR   PIC  X(0030).
             10 PI-DRKI2365-FSAGEMFREIBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSAGUELTIGAB   PIC  X(010).
             10 PI-DRKI2365-FSAKISTEINBEHALT   PIC  X(010).
             10 PI-DRKI2365-FSAKISTSATZ1   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2365-FSAKISTSATZ2   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2365-FSAKONFESSION1   PIC  X(0002).
             10 PI-DRKI2365-FSAKONFESSION2   PIC  X(0002).
             10 PI-DRKI2365-FSAKZSTAUSL   PIC  X(0001).
             10 PI-DRKI2365-FSAKZVERBAKTIEN   PIC  X(0001).
             10 PI-DRKI2365-FSAKZVERBSONST   PIC  X(0001).
             10 PI-DRKI2365-FSANVGUELTIGBIS   PIC  X(010).
             10 PI-DRKI2365-FSAPARTNERGESID   PIC S9(4) COMP.
             10 PI-DRKI2365-FSASTUECKZINS   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSASUMERTRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSASUMKEST   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSASUMKIST   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSASUMSOLI   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSAVERBRFREIVORJ   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSAVERLUSTAKTIEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSAVERLUSTSONST   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2365-FSAWAEHRUNGSKZ   PIC  X(0001).
             10 PI-DRKI2365-FSAZINSABSCHSTEUER   PIC S9(9)V9(2) COMP-3.
END   *

      *
       01  PO-DRKI2365.
           05  PO-DRKI2365-RC                     PIC  X(003).
               88 PO-DRKI2365-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2365-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
