      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.12.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3766.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3766.CPv  $
      *
      *   Rev 2.1   Jan 09 2020 13:53:02   t13ex39
      *AKI01747: EU-DSGVO: Vertragslöschung der Deutschen Bank
      *
      *   Rev 2.0   Dec 12 2019 07:31:48 T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3766                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3766                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.12.2019 / 07:31:48 / T13EX39
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
      *    FÜR DIE FUNKTION DRKI3766
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3766.
           05  PI-DRKI3766-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI3766-GVDGESELLSCHAFT   PIC S9(4) COMP.
             10 PI-DRKI3766-GVDLAUFENDENUMMER   PIC S9(4) COMP.
             10 PI-DRKI3766-GVDVERTRAGSNUMMER   PIC  X(0015).
END   *

           05  PI-DRKI3766-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI3766-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI3766-GVDDATUM   PIC  X(010).
             10 PI-DRKI3766-GVDVERTRAGSABLAUF   PIC  X(010).
             10 PI-DRKI3766-GVDVERTRAGSID   PIC  X(0013).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI3766-NI1.
           05 PI-DRKI3766-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI3766-NI7.
           05 PI-DRKI3766-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI3766.
           05  PO-DRKI3766-RC                     PIC  X(003).
               88 PO-DRKI3766-RC-OK               VALUE 'OK '.
               88 PO-DRKI3766-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

