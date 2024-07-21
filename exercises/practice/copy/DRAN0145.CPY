      ****************************************************************
      *    COPY-MEMBER: DRAN0145 WORKFLOW ANTRAG NEU                 *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00655!13.09.06!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      * AAN00655!28.11.06!2.2 ! TIMESTAMP                 ! HEIDER   *
      * AAN01033!        !2.3 ! ERWEITERUNG DRAN0141      ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      *****************************************************************
      * LOGBUCH:
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME
      * --------------------------------------------------------------*
      * ANTRAG-1928!01.02.23! C+L WG. DRAN0141             ! RAHNER
      *            !        !                              !
      *            !        !                              !
      *            !        !                              !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      ****************************************************************
      *                                                              *
      *    DIESES COPY IST EIN WORKFLOW DER ANTRAG-DATEN             *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *!!!ACHTUNG!!! -> BEI ÄNDERUNG AUCH DRAN0132 ANPASSEN!!!       *
      ****************************************************************
       01  WF-ANTRAG-NEU.
           05 WF-ANTRAG-DATEN-NEU.
             10 WF-ANTRAG-KOPF-NEU.
                COPY DRAN0141.
      *
             10 WF-ANTRAG-PE-STRUKTUR-NEU.
                COPY DRAN0142.
      *
             10 WF-ANTRAG-PRODUKTE-NEU.
                15 WF-ANTRAG-PRODUKT-TABELLE-NEU OCCURS 15.
                COPY DRAN0143.

      *         TIMESTAMP WORKFLOW
             10 WF-ANTA-LAST-UPDATE-NEU    PIC X(26).
      *
      *         ZU VERARBEITENDE DATEN AUS ELEKTRONISCHEN VORSCHLÄGEN
             10 WF-ANTRAG-VORSCHLAG-NEU    PIC X(1000).
      *         -> ZUSATZDATEN JE NACH PRODUKTGEBER
      *
      ******** ENDE COPY-MEMBER **************************************
