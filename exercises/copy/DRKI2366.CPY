      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.10.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2366.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2366.CPv  $
      *
      *   Rev 2.2   Jun 28 2011 09:55:24   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g FSA
      *
      *   Rev 2.1   Nov 16 2009 18:29:40   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Einführung für Deutsche Bank
      *
      *   Rev 2.0   Oct 19 2009 12:23:00   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2366                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2366                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.06.2011 / 10:55:22 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2366
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2366.
           05  PI-DRKI2366-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2366-FSAPERSONENID   PIC  X(0013).
             10 PI-DRKI2366-FSAVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2366-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2366-FSAANZDEPFREISTBET   PIC S9(4) COMP.
             10 PI-DRKI2366-FSAAUFTKONF1   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2366-NI-FSAAUFTKONF1    PIC S9(4) COMP.
             10 PI-DRKI2366-FSAAUFTKONF2   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2366-FSAAUSLQSTEUER   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSAFREIBETRAGABL   PIC  X(010).
             10 PI-DRKI2366-FSAFREIBETRAGVERBR   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSAFREISTAUFTRNR   PIC  X(0030).
             10 PI-DRKI2366-FSAGEMFREIBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSAGUELTIGAB   PIC  X(010).
             10 PI-DRKI2366-FSAKISTEINBEHALT   PIC  X(010).
             10 PI-DRKI2366-FSAKISTSATZ1   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2366-FSAKISTSATZ2   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2366-FSAKONFESSION1   PIC  X(0002).
             10 PI-DRKI2366-FSAKONFESSION2   PIC  X(0002).
             10 PI-DRKI2366-FSAKZSTAUSL   PIC  X(0001).
             10 PI-DRKI2366-FSAKZVERBAKTIEN   PIC  X(0001).
             10 PI-DRKI2366-FSAKZVERBSONST   PIC  X(0001).
             10 PI-DRKI2366-FSANVGUELTIGBIS   PIC  X(010).
             10 PI-DRKI2366-FSAPARTNERGESID   PIC S9(4) COMP.
             10 PI-DRKI2366-FSASTUECKZINS   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSASUMERTRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSASUMKEST   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSASUMKIST   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSASUMSOLI   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSAVERBRFREIVORJ   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSAVERLUSTAKTIEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSAVERLUSTSONST   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2366-FSAWAEHRUNGSKZ   PIC  X(0001).
             10 PI-DRKI2366-FSAZINSABSCHSTEUER   PIC S9(9)V9(2) COMP-3.
END   *


      *
       01  PO-DRKI2366.
           05  PO-DRKI2366-RC                     PIC  X(003).
               88 PO-DRKI2366-RC-OK               VALUE 'OK '.
               88 PO-DRKI2366-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
