      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.11.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2368.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2368.CPv  $
      *
      *   Rev 2.2   Jun 28 2011 09:55:30   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g FSA
      *
      *   Rev 2.1   Nov 16 2009 18:30:02   t13ex51
      *AKI00866: Abgeltungssteuerdaten: Einführung für Deutsche Bank
      *
      *   Rev 2.0   Nov 10 2009 12:04:10   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2368                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2368                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         07.06.2011 / 10:55:26 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2368
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2368.
           05  PI-DRKI2368-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2368-FSAPERSONENID   PIC  X(0013).
             10 PI-DRKI2368-FSAVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2368.
           05  PO-DRKI2368-RC                     PIC  X(003).
               88 PO-DRKI2368-RC-OK               VALUE 'OK '.
               88 PO-DRKI2368-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2368-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2368-FSAANZDEPFREISTBET   PIC S9(4) COMP.
             10 PO-DRKI2368-FSAAUFTKONF1   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI2368-NI-FSAAUFTKONF1   PIC S9(4) COMP.
             10 PO-DRKI2368-FSAAUFTKONF2   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI2368-FSAAUSLQSTEUER   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSAFREIBETRAGABL   PIC  X(010).
             10 PO-DRKI2368-FSAFREIBETRAGVERBR   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSAFREISTAUFTRNR   PIC  X(0030).
             10 PO-DRKI2368-FSAGEMFREIBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSAGUELTIGAB   PIC  X(010).
             10 PO-DRKI2368-FSAKISTEINBEHALT   PIC  X(010).
             10 PO-DRKI2368-FSAKISTSATZ1   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI2368-FSAKISTSATZ2   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI2368-FSAKONFESSION1   PIC  X(0002).
             10 PO-DRKI2368-FSAKONFESSION2   PIC  X(0002).
             10 PO-DRKI2368-FSAKZSTAUSL   PIC  X(0001).
             10 PO-DRKI2368-FSAKZVERBAKTIEN   PIC  X(0001).
             10 PO-DRKI2368-FSAKZVERBSONST   PIC  X(0001).
             10 PO-DRKI2368-FSANVGUELTIGBIS   PIC  X(010).
             10 PO-DRKI2368-FSAPARTNERGESID   PIC S9(4) COMP.
             10 PO-DRKI2368-FSASTUECKZINS   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSASUMERTRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSASUMKEST   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSASUMKIST   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSASUMSOLI   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSAVERBRFREIVORJ   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSAVERLUSTAKTIEN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSAVERLUSTSONST   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-FSAWAEHRUNGSKZ   PIC  X(0001).
             10 PO-DRKI2368-FSAZINSABSCHSTEUER   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2368-LETZTEAENDERUNG   PIC  X(026).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
