      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.01.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2442.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2442.CPv  $
      *
      *   Rev 2.2   Apr 23 2012 12:46:42   t13ex48
      *AKI01117: DB-Änderungen R. 6.16: OZPs, COPYs TCKI0077
      *
      *   Rev 2.1   Jan 31 2011 09:53:34   t13ex27
      *AKI00955: ID- OKI pausch KÜ: Ablehnung bei mehreren VN
      *
      *   Rev 2.0   Jan 10 2011 16:03:58   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2442                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2442                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.01.2011 / 16:06:58 / T13EX27
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
230412* AKI01117!23.04.12!2.6 ! Spalte KDUNOTIZ entfernt  !HSE       *
      * -------------------------------------------------------------*
      * DBO-9310!12.10.21!    ! KDUBEARBEITER von 8 auf 30! t13ex78  *
      *         !        !    ! Stellen erweitert         !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2442
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2442.
           05  PI-DRKI2442-KEY.


      *        %M_PI_KEY%
             10 PI-DRKI2442-VERTRAGSID   PIC  X(0013).
      *

      *
       01  PO-DRKI2442.
           05  PO-DRKI2442-RC                     PIC  X(003).
               88 PO-DRKI2442-RC-OK               VALUE 'OK '.
               88 PO-DRKI2442-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2442-DATEN.


      *        %M_PO_DATEN%
             10 PO-DRKI2442-DTAERLEDIGTKZ   PIC  X(0001).
             10 PO-DRKI2442-KDUBEARBEITER   PIC  X(0030).
             10 PO-DRKI2442-KDUEBERTRAGUNGSART   PIC  X(0001).
             10 PO-DRKI2442-KDUEIGENVERMKZ   PIC  X(0001).
             10 PO-DRKI2442-KDUGUELTIGABDAT   PIC  X(010).
             10 PO-DRKI2442-KDUKKVLID   PIC  X(0013).
             10 PO-DRKI2442-KDULAUFENDENUMMER   PIC S9(4) COMP.
230412*      10 PO-DRKI2442-KDUNOTIZ.
230412*         49 PO-DRKI2442-KDUNOTIZ-LEN PIC S9(4) COMP.
230412*         49 PO-DRKI2442-KDUNOTIZ-TEXT PIC X(60).
             10 PO-DRKI2442-LETZTEAENDERUNG   PIC  X(026).
             10 PO-DRKI2442-VBALTID   PIC  X(0013).
             10 PO-DRKI2442-VBALTROLLENKZ   PIC  X(0001).
             10 PO-DRKI2442-VBNEUID   PIC  X(0013).
             10 PO-DRKI2442-VBNEUROLLENKZ   PIC  X(0001).
      *


      *
      ******* ENDE COPY-MEMBER ***************************************
