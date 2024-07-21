      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0252.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0252.CPv  $
      *
      *   Rev 2.1   May 20 2003 12:16:06   t13ae34
      *AAN00443 - autom. Pflegen Vorgabe Strukturerfassungsdatum 
      *
      *   Rev 2.0   Feb 27 1999 14:28:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   11 Aug 1998 09:29:32   T13EX16
      *APA00032 - PEW R2.0
      *
      *   Rev 1.1   Apr 09 1997 16:55:08   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Apr 03 1997 16:23:26   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0252                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJPA0009                    *
      *    - ENTHäLT DIE FUNKTIONEN                                   *
      *      DRPA0253 (TAGESDATUM UND TIMESTAMP ERMITTELN)            *
200198*      DRPA0808 (INDUSTRIETAGE ERMITTELN)                       *
160298*      DRPA0857 (DATUM SETZEN)                                  *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    02.04.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !02.04.97!01  ! NEU                       ! STORCH   *
      *         !20.01.98!    ! NEUE FUNKTION EINGEFüGT   ! STORCH   *
      *         !04.05.98!    ! NEUE FUNKTION EINGEFÜGT   ! MUND     *
      * AAN00443!20.05.03!2.1 ! DRPA1560                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.   NUR DIE FUNKTIONEN
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZJPA0009.
           05  PI-ZJPA0009-FUNKTION            PIC  X(008).
               88 PI-ZJPA0009-DRPA0253              VALUE 'DRPA0253'.
200198         88 PI-ZJPA0009-DRPA0808              VALUE 'DRPA0808'.
160298         88 PI-ZJPA0009-DRPA0857              VALUE 'DRPA0857'.
040598         88 PI-ZJPA0009-DRPA0946              VALUE 'DRPA0946'.
200503         88 PI-ZJPA0009-DRPA1560              VALUE 'DRPA1560'.
      *
      ******* ENDE COPY-MEMBER **************************************
