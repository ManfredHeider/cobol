      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1259.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1259.CPv  $
      *
      *   Rev 2.1   May 03 2006 12:32:54   t13ex39
      *AKI00215: Umstellung auf EURO UMSATZ
      *
      *   Rev 2.0   05 Jul 2001 17:06:56   p13sp99
      *- Neuerstellung -
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1259                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI1259                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  24.07.01/14.45.35
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER
      *    FšR DIE FUNKTION DRKI1259
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1259.
           05  PI-DRKI1259-KEY.


             10 PI-DRKI1259-UMSGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1259-UMSLAUFENDENR   PIC S9(4) COMP.
             10 PI-DRKI1259-UMSVERTRAGSID   PIC X(13).


           05  PI-DRKI1259-DATEN.


             10 PI-DRKI1259-UMSBUCHUNGSDATUM   PIC X(10).
             10 PI-DRKI1259-UMSTEXT.
                15 PI-DRKI1259-UMSTEXT-LEN   PIC S9(4) COMP.
                15 PI-DRKI1259-UMSTEXT-TEXT   PIC X(20).

             10 PI-DRKI1259-UMSTEXTID   PIC S9(4) COMP.
             10 PI-DRKI1259-UMSUMSATZBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1259-NI-UMSUMSATZBETRAG   PIC S9(4) COMP.
             10 PI-DRKI1259-UMSUMSBETRWKZ   PIC X(01).
             10 PI-DRKI1259-UMSZAHLUNGSWEISEID   PIC S9(4) COMP.
             10 PI-DRKI1259-N-MSZHLNGSWSD   PIC S9(4) COMP.


      *
       01  PO-DRKI1259.
           05  PO-DRKI1259-RC                 PIC  X(003).
               88 PO-DRKI1259-RC-OK                        VALUE 'OK '.
               88 PO-DRKI1259-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
