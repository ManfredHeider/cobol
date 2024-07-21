      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2724.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2724.CPv  $
      *
      *   Rev 2.3   Jun 13 2013 13:06:42   t13ex27
      *AKI01204: SEPA - DB-Online - OZP für BTGZAHLERVTG
      *
      *   Rev 2.2   Jun 06 2013 09:52:00   t13ex14
      *AKI01204: SEPA - DB-Online - OZP für BTGZAHLERVTG
      *
      *   Rev 2.1   Feb 27 2013 13:57:42   t13ex14
      *AKI01204: SEPA - DB-Online - OZPs für SEPA-Tabellen
      *
      *   Rev 2.0   Feb 27 2013 11:38:13 T13EX14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2724                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2724                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.06.2013 / 12:04:44 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI2724
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2724.
           05  PI-DRKI2724-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2724-BZVLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2724-BZVVERTRAGSID   PIC  X(0013).
END   *


           05  PI-DRKI2724-DATEN.


ANF   *        %M_PI_DATEN%
             10 PI-DRKI2724-BZVABWBTGZAHLER   PIC  X(0001).
             10 PI-DRKI2724-BZVADRESSEID   PIC  X(0013).
             10 PI-DRKI2724-BZVANREDE-KNR   PIC S9(4) COMP.
             10 PI-DRKI2724-BZVANREDE-KTY   PIC S9(4) COMP.
             10 PI-DRKI2724-BZVBIC   PIC  X(0011).
             10 PI-DRKI2724-BZVGEBDATUM   PIC  X(010).
             10 PI-DRKI2724-BZVIBAN   PIC  X(0034).
             10 PI-DRKI2724-BZVKNDPERSONENID   PIC  X(0013).
             10 PI-DRKI2724-BZVMANDATREFNR.
                49 PI-DRKI2724-BZVMANDATREFNR-LEN PIC S9(4) COMP.
                49 PI-DRKI2724-BZVMNDTRFNR-TEXT PIC X(35).
             10 PI-DRKI2724-BZVNACHNAME   PIC  X(0030).
             10 PI-DRKI2724-BZVVORNAME   PIC  X(0030).
END   *

      *
       01  PO-DRKI2724.
           05  PO-DRKI2724-RC                     PIC  X(003).
               88 PO-DRKI2724-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2724-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
