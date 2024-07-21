      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0692.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0692.CPv  $
      *
      *   Rev 2.3   Jul 12 2010 08:49:46   t13ex14
      *AKI00967: Badenia: OZPs und Copies
      *
      *   Rev 2.2   Jun 24 2010 11:58:48   t13ex14
      *AKI00967: Badenia: OZPs und Copies
      *
      *   Rev 2.1   Apr 08 1999 16:23:58   T13EX51
      *AKI00021
      *
      *   Rev 2.0   Mar 01 1999 15:07:30   T13EX51
      *Neue Revision AKI000001
      *
      *   Rev 1.0   25 Aug 1998 09:31:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0692                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0692                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
020610* AKI00967!02.06.10!    ! neues Feld UMSVWLUMSATZKZ ! WMI      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0692
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0692.
           05  PI-DRKI0692-KEY.


             10 PI-DRKI0692-UMSGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0692-UMSLAUFENDENR   PIC S9(4) COMP.
             10 PI-DRKI0692-UMSVERTRAGSID   PIC X(13).


           05  PI-DRKI0692-DATEN.


             10 PI-DRKI0692-UMSBUCHUNGSDATUM   PIC X(10).
             10 PI-DRKI0692-UMSTEXT.
                15 PI-DRKI0692-UMSTEXT-LEN   PIC S9(4) COMP.
                15 PI-DRKI0692-UMSTEXT-TEXT   PIC X(20).

             10 PI-DRKI0692-UMSTEXTID   PIC S9(4) COMP.
             10 PI-DRKI0692-UMSUMSATZBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0692-UMSUMSBETRWKZ   PIC X(01).
             10 PI-DRKI0692-UMSZAHLUNGSWEISEID   PIC S9(4) COMP.
020610       10 PI-DRKI0692-UMSVWLUMSATZKZ       PIC X(01).


      *
       01  PO-DRKI0692.
           05  PO-DRKI0692-RC                 PIC  X(003).
               88 PO-DRKI0692-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0692-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
