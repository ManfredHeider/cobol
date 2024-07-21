      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2723.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2723.CPv  $
      *
      *   Rev 2.3   Jun 13 2013 13:06:36   t13ex27
      *AKI01204: SEPA - DB-Online - OZP für BTGZAHLERVTG
      *
      *   Rev 2.2   Jun 06 2013 09:50:02   t13ex14
      *AKI01204: SEPA - DB-Online - OZP für BTGZAHLERVTG
      *
      *   Rev 2.1   Feb 27 2013 13:57:34   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs für SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:38:08 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2723                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2723                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.06.2013 / 12:04:42 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2723
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2723.
           05  PI-DRKI2723-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2723-BZVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2723-BZVVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2723.
           05  PO-DRKI2723-RC                     PIC  X(003).
               88 PO-DRKI2723-RC-OK               VALUE 'OK '.
               88 PO-DRKI2723-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2723-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2723-BZVABWBTGZAHLER   PIC  X(0001).
             10 PO-DRKI2723-BZVADRESSEID   PIC  X(0013).
             10 PO-DRKI2723-BZVANREDE-KNR   PIC S9(4) COMP.
             10 PO-DRKI2723-BZVANREDE-KTY   PIC S9(4) COMP.
             10 PO-DRKI2723-BZVBIC   PIC  X(0011).
             10 PO-DRKI2723-BZVGEBDATUM   PIC  X(010).
             10 PO-DRKI2723-BZVIBAN   PIC  X(0034).
             10 PO-DRKI2723-BZVKNDPERSONENID   PIC  X(0013).
             10 PO-DRKI2723-BZVMANDATREFNR.
                49 PO-DRKI2723-BZVMANDATREFNR-LEN PIC S9(4) COMP.
                49 PO-DRKI2723-BZVMNDTRFNR-TEXT PIC X(35).
             10 PO-DRKI2723-BZVNACHNAME   PIC  X(0030).
             10 PO-DRKI2723-BZVVORNAME   PIC  X(0030).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
