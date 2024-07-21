      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2725.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2725.CPv  $
      *
      *   Rev 2.3   Jun 13 2013 13:06:46   t13ex27
      *AKI01204: SEPA - DB-Online - OZP für BTGZAHLERVTG
      *
      *   Rev 2.2   Jun 06 2013 09:53:58   t13ex14
      *AKI01204: SEPA - DB-Online - OZP für BTGZAHLERVTG
      *
      *   Rev 2.1   Feb 27 2013 13:57:48   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs für SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:38:17 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2725                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2725                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.06.2013 / 12:04:46 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2725
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2725.
           05  PI-DRKI2725-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2725-BZVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2725-BZVVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2725-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2725-BZVABWBTGZAHLER   PIC  X(0001).
             10 PI-DRKI2725-BZVADRESSEID   PIC  X(0013).
             10 PI-DRKI2725-BZVANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2725-BZVANREDE-KTY   PIC S9(4) COMP.
             10 PI-DRKI2725-BZVBIC   PIC  X(0011).
             10 PI-DRKI2725-BZVGEBDATUM   PIC  X(010).
             10 PI-DRKI2725-BZVIBAN   PIC  X(0034).
             10 PI-DRKI2725-BZVKNDPERSONENID   PIC  X(0013).
             10 PI-DRKI2725-BZVMANDATREFNR.
                49 PI-DRKI2725-BZVMANDATREFNR-LEN PIC S9(4) COMP.
                49 PI-DRKI2725-BZVMNDTRFNR-TEXT PIC X(35).
             10 PI-DRKI2725-BZVNACHNAME   PIC  X(0030).
             10 PI-DRKI2725-BZVVORNAME   PIC  X(0030).
END   *


      *
       01  PO-DRKI2725.
           05  PO-DRKI2725-RC                     PIC  X(003).
               88 PO-DRKI2725-RC-OK               VALUE 'OK '.
               88 PO-DRKI2725-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
